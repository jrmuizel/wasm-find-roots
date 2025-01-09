use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Cursor, Write};
use std::path::Path;
use regex::Regex;
use crate::parse_cc_graph::{parse_cc_edge_buf, parse_cc_edge_file, to_single_graph, GraphAttribs, WeakMapEntry};

// override print! and println! to write into a global vec
use std::fmt::Write as FmtWrite;
use std::sync::Mutex;
lazy_static::lazy_static! {
    static ref OUTPUT: Mutex<String> = Mutex::new(String::new());
}
macro_rules! print {
    ($($arg:tt)*) => {
        {let mut output = OUTPUT.lock().unwrap();
        write!(output, $($arg)*).unwrap();
        }
    };
}
macro_rules! println {
    () => {
        {let mut output = OUTPUT.lock().unwrap();
        writeln!(output).unwrap();
        }
    };
    ($($arg:tt)*) => {
        {let mut output = OUTPUT.lock().unwrap();
        writeln!(output, $($arg)*).unwrap();
        }
    };
}


struct Args {
    file_name: String,
    target: String,
    simple_path: bool,
    print_reverse: bool,
    ignore_rc_roots: bool,
    ignore_js_roots: bool,
    node_roots: Option<String>,
    print_roots_only: bool,
    output_to_file: bool,
    weak_maps: bool,
    weak_maps_maps_live: bool,
    use_dfs: bool,
    hide_weak_maps: bool,
    output_file: Box<std::io::Cursor<Vec<u8>>>,
}


use clap::{Arg, ArgAction, Command};


impl Args {
    /*fn new() -> Self {
        let matches = Command::new("find_roots")
            .arg(Arg::new("file_name")
                .required(true)
                .help("cycle collector graph file name"))
            .arg(Arg::new("target")
                .required(true)
                .help("address of target object or prefix of class name of targets"))
            .arg(Arg::new("simple_path")
                .short('s')
                .long("simple-path")
                .action(ArgAction::SetTrue)
                .help("Print paths on a single line and remove addresses to help large-scale analysis of paths."))
            .arg(Arg::new("print_reverse")
                .short('r')
                .long("print-reverse")
                .action(ArgAction::SetTrue)
                .help("Display paths in simple mode going from destination to source, rather than from source to destination."))
            .arg(Arg::new("ignore_rc_roots")
                .short('i')
                .long("ignore-rc-roots")
                .action(ArgAction::SetTrue)
                .help("ignore ref counted roots"))
            .arg(Arg::new("ignore_js_roots")
                .short('j')
                .long("ignore-js-roots")
                .action(ArgAction::SetTrue)
                .help("ignore Javascript roots"))
            .arg(Arg::new("node_roots")
                .short('n')
                .long("node-name-as-root")
                .action(ArgAction::Set)
                .help("treat nodes with this class name as extra roots"))
            .arg(Arg::new("print_roots_only")
                .long("print-roots-only")
                .action(ArgAction::SetTrue)
                .help("Only print out the addresses of rooting objects, to simplify use from other programs."))
            .arg(Arg::new("output_to_file")
                .long("output-to-file")
                .action(ArgAction::SetTrue)
                .help("Print the results to a file. This only works correctly with --print-roots-only."))
            .arg(Arg::new("weak_maps")
                .long("weak-maps")
                .action(ArgAction::SetTrue)
                .help("Enable experimental weak map support in DFS mode. WARNING: this may not give accurate results."))
            .arg(Arg::new("weak_maps_maps_live")
                .long("weak-maps-maps-live")
                .action(ArgAction::SetTrue)
                .help("Pretend all weak maps are alive in DFS mode. Implies --weak-maps. WARNING: this may not give accurate results."))
            .arg(Arg::new("use_dfs")
                .short('d')
                .long("depth-first")
                .action(ArgAction::SetTrue)
                .help("Use the old depth-first algorithm for finding paths."))
            .arg(Arg::new("hide_weak_maps")
                .long("hide-weak-maps")
                .action(ArgAction::SetTrue)
                .help("If selected, don't show why any weak maps in the path are alive."))
            .get_matches();

        let output_file: Box<dyn std::io::Write> = if matches.get_flag("output_to_file") {
            Box::new(std::fs::File::create(format!("{}.out", matches.get_one::<String>("file_name").unwrap())).unwrap())
        } else {
            Box::new(std::io::stdout())
        };

        Args {
            file_name: matches.get_one::<String>("file_name").unwrap().to_string(),
            target: matches.get_one::<String>("target").unwrap().to_string(),
            simple_path: matches.get_flag("simple_path"),
            print_reverse: matches.get_flag("print_reverse"),
            ignore_rc_roots: matches.get_flag("ignore_rc_roots"),
            ignore_js_roots: matches.get_flag("ignore_js_roots"),
            node_roots: matches.get_one::<String>("node_roots").map(|s| s.to_string()),
            print_roots_only: matches.get_flag("print_roots_only"),
            output_to_file: matches.get_flag("output_to_file"),
            weak_maps: matches.get_flag("weak_maps"),
            weak_maps_maps_live: matches.get_flag("weak_maps_maps_live"),
            use_dfs: matches.get_flag("use_dfs"),
            hide_weak_maps: matches.get_flag("hide_weak_maps"),
            output_file,
        }
    }*/
    fn default() -> Self {
        Args {
            file_name: "".to_string(),
            target: "".to_string(),
            simple_path: false,
            print_reverse: false,
            ignore_rc_roots: false,
            ignore_js_roots: false,
            node_roots: None,
            print_roots_only: false,
            output_to_file: false,
            weak_maps: false,
            weak_maps_maps_live: false,
            use_dfs: false,
            hide_weak_maps: false,
            output_file: Box::new(Cursor::new(Vec::new())),
        }
    }
}


fn print_node(ga: &GraphAttribs, x: &str) {
    print!("{} [{}]", x, ga.node_labels.get(x).unwrap_or(&"".to_string()));
}

fn print_edge(args: &Args, ga: &GraphAttribs, x: &str, y: &str) {
    fn print_edge_label(l: &str) {
        if l.len() == 2 {
            print!("{}", &l[0..1]);
        } else {
            print!("{}", l);
        }
    }

    if args.print_reverse {
        print!("<--[");
    } else {
        print!("--[");
    }

    if let Some(labels) = ga.edge_labels.get(x).and_then(|m| m.get(y)) {
        if !labels.is_empty() {
            print_edge_label(&labels[0]);
            for l in &labels[1..] {
                print!(", ");
                print_edge_label(l);
            }
        }
    }

    if args.print_reverse {
        print!("]--");
    } else {
        print!("]-->");
    }
}

fn print_known_edges(args: &Args, known_edges: &[String], ga: &GraphAttribs, x: &str) {
    if known_edges.is_empty() {
        return;
    }

    println!("    known edges:");
    for e in known_edges {
        print!("       ");
        print_node(ga, e);
        print!(" ");
        print_edge(args, ga, e, x);
        println!(" {}", x);
    }
}

fn explain_root(args: &Args, known_edges_fn: &dyn Fn(&str) -> Vec<String>, ga: &GraphAttribs, num_known: &HashMap<String, u32>, roots: &HashMap<String, String>, root: &str) {
    print!("    Root {} ", root);

    if roots[root] == "gcRoot" {
        println!("is a marked GC object.");
        if ga.incr_roots.contains(root) {
            println!("    It is an incremental root, which means it was touched during an incremental CC.");
        }
        return;
    } else if roots[root] == "stopNodeLabel" {
        println!("is an extra root class.");
        return;
    }

    assert_eq!(roots[root], "rcRoot");

    let num_unknown = if let Some(k) = num_known.get(root) {
        ga.rc_nodes.get(root).unwrap_or(&0) - k
    } else {
        assert!(ga.incr_roots.contains(root));
        0
    };

    println!("is a ref counted object with {} unknown edge(s).", num_unknown);

    print_known_edges(args, &known_edges_fn(root), ga, root);

    if ga.incr_roots.contains(root) {
        println!("    It is an incremental root, which means it was touched during an incremental CC.");
    }
}

fn print_path_basic(args: &Args, known_edges_fn: &dyn Fn(&str) -> Vec<String>, ga: &GraphAttribs, num_known: &HashMap<String, u32>, roots: &HashMap<String, String>, path: &[String]) {
    print_node(ga, &path[0]);
    println!();
    let mut prev = &path[0];

    for p in &path[1..] {
        print!("    ");
        print_edge(args, ga, prev, p);
        print!(" ");
        print_node(ga, p);
        println!();
        prev = p;
    }

    println!();

    explain_root(args, known_edges_fn, ga, num_known, roots, &path[0]);
    println!();
}

fn print_simple_node(ga: &GraphAttribs, x: &str) {
    print!("[{}]", ga.node_labels.get(x).unwrap_or(&"".to_string()));
}

fn print_simple_path(args: &Args, ga: &GraphAttribs, path: &[String]) {
    print_simple_node(ga, &path[0]);
    let mut prev = &path[0];

    for p in &path[1..] {
        print!(" ");
        print_edge(args, ga, prev, p);
        print!(" ");
        print_simple_node(ga, p);
        prev = p;
    }

    println!();
}

fn print_roots_only_path(f: &mut dyn Write, path: &[String]) {
    writeln!(f, "{}", path[0]).unwrap();
}

fn print_path(args: &mut Args, known_edges_fn: &dyn Fn(&str) -> Vec<String>, ga: &GraphAttribs, num_known: &HashMap<String, u32>, roots: &HashMap<String, String>, path: &[String]) {
    if args.print_roots_only {
        print_roots_only_path(&mut args.output_file, path);
    } else if args.simple_path {
        if args.print_reverse {
            let mut reversed_path = path.to_vec();
            reversed_path.reverse();
            print_simple_path(args, ga, &reversed_path);
        } else {
            print_simple_path(args, ga, path);
        }
    } else {
        print_path_basic(args, known_edges_fn, ga, num_known, roots, path);
    }
}

fn find_roots_bfs(args: &mut Args, g: &HashMap<String, HashSet<String>>, ga: &mut GraphAttribs, num_known: &HashMap<String, u32>, roots: &HashMap<String, String>, target: &str) {
    let mut work_list = VecDeque::new();
    let mut distances = HashMap::new();
    let mut limit = -1;

    fn traverse_weak_map_entry(dist: i32, k: &str, m: &str, v: &str, lbl: &str, distances: &mut HashMap<String, (i32, String, Option<(String, String)>)>, work_list: &mut VecDeque<String>) {
        if !distances.contains_key(k) || !distances.contains_key(m) {
            return;
        }

        if distances[k].0 > dist || distances[m].0 > dist {
            return;
        }

        if distances.contains_key(v) {
            assert!(distances[v].0 <= dist + 1);
            return;
        }

        distances.insert(v.to_string(), (dist + 1, k.to_string(), Some((m.to_string(), lbl.to_string()))));
        work_list.push_back(v.to_string());
    }

    let mut weak_data = HashMap::new();
    for wme in &ga.weak_map_entries {
        weak_data.entry(wme.weak_map.clone()).or_insert_with(HashSet::new).insert(wme);
        weak_data.entry(wme.key.clone()).or_insert_with(HashSet::new).insert(wme);
        if wme.key_delegate != "0x0" {
            weak_data.entry(wme.key_delegate.clone()).or_insert_with(HashSet::new).insert(wme);
        }
    }

    let start_object = "FAKE START OBJECT";
    let mut root_edges = HashSet::new();
    for r in roots.keys() {
        root_edges.insert(r.clone());
    }

    assert!(!g.contains_key(start_object));
    let mut g = g.clone();
    g.insert(start_object.to_string(), root_edges);
    distances.insert(start_object.to_string(), (-1, "".to_string(), None));
    work_list.push_back(start_object.to_string());

    while let Some(x) = work_list.pop_front() {
        let dist = distances[&x].0;

        assert!(dist >= limit);
        limit = dist;

        if x == target {
            continue;
        }

        if !g.contains_key(&x) {
            continue;
        }

        let new_dist = dist + 1;
        let new_dist_node = (new_dist, x.clone(), None);

        for y in &g[&x] {
            if let Some(d) = distances.get(y) {
                assert!(d.0 <= new_dist);
            } else {
                distances.insert(y.clone(), new_dist_node.clone());
                work_list.push_back(y.clone());
            }
        }

        if let Some(wmes) = weak_data.get(&x) {
            for wme in wmes {
                assert!(x == wme.weak_map || x == wme.key || x == wme.key_delegate);
                traverse_weak_map_entry(dist, &wme.key, &wme.weak_map, &wme.value, &format!("value in weak map {}", wme.weak_map), &mut distances, &mut work_list);
                traverse_weak_map_entry(dist, &wme.key_delegate, &wme.weak_map, &wme.key, &format!("key delegate in weak map {}", wme.weak_map), &mut distances, &mut work_list);
            }
        }
    }

    let mut print_work_list = VecDeque::new();
    print_work_list.push_back(target.to_string());
    let mut printed_things = HashSet::new();
    printed_things.insert(target.to_string());

    fn known_edges_fn(g: &HashMap<String, HashSet<String>>, node: &str) -> Vec<String> {
        let mut known_edges = Vec::new();
        for (src, dsts) in g {
            if dsts.contains(node) && src != "FAKE START OBJECT" {
                known_edges.push(src.clone());
            }
        }
        known_edges
    }

    while let Some(p) = print_work_list.pop_front() {
        let mut path = Vec::new();
        let mut current = p.clone();
        while let Some(dist) = distances.get(&current) {
            path.push(current.clone());
            match dist {
                (_, _, None) => {
                current = dist.1.clone();
                }
                (_, k, Some((m, lbl))) => {
                    ga.edge_labels.get_mut(k).and_then(|m| m.get_mut(&current)).map(|labels| labels.push(lbl.clone()));
                    current = k.clone();
                    if !printed_things.contains(m) && !args.hide_weak_maps {
                        print_work_list.push_back(m.clone());
                        printed_things.insert(m.clone());
                    }
                }
            }
        }

        if !path.is_empty() {
            assert_eq!(path[path.len() - 1], start_object);
            path.pop();
            path.reverse();

            println!();

            print_path(args, &|node| known_edges_fn(&g, node), ga, num_known, roots, &path);
        } else {
            println!("Didn't find a path.");
            println!();
            print_known_edges(args, &known_edges_fn(&g, &p), ga, &p);
        }
    }

    g.remove(start_object);
}

fn reverse_graph(g: &HashMap<String, HashSet<String>>) -> HashMap<String, HashSet<String>> {
    let mut g2 = HashMap::new();
    print!("Reversing graph. ");
    for (src, dsts) in g {
        for d in dsts {
            g2.entry(d.clone()).or_insert_with(HashSet::new).insert(src.clone());
        }
    }
    println!("Done.\n");
    g2
}

fn reverse_graph_known_edges(revg: &HashMap<String, HashSet<String>>, target: &str) -> Vec<String> {
    let mut known = Vec::new();
    if let Some(edges) = revg.get(target) {
        for x in edges {
            known.push(x.clone());
        }
    }
    known
}

fn pretend_about_weak_maps(args: &Args, g: &mut HashMap<String, HashSet<String>>, ga: &mut GraphAttribs) {
    fn null_to_none(s: &str) -> Option<String> {
        if s == "0x0" {
            None
        } else {
            Some(s.to_string())
        }
    }

    for wme in &ga.weak_map_entries {
        let m = null_to_none(&wme.weak_map);
        let k = null_to_none(&wme.key);
        let kd = null_to_none(&wme.key_delegate);
        let v = null_to_none(&wme.value);

        if m.is_some() && !args.weak_maps_maps_live {
            continue;
        }
        if kd.is_some() {
            continue;
        }
        if k.is_none() {
            continue;
        }
        if v.is_none() {
            continue;
        }

        g.entry(k.clone().unwrap()).or_insert_with(HashSet::new).insert(v.clone().unwrap());

        let edge_label = if m.is_some() {
            format!("weak map key-value edge in map {}", m.unwrap())
        } else {
            "weak map key-value edge in black map".to_string()
        };

        ga.edge_labels.entry(k.clone().unwrap()).or_insert_with(HashMap::new).entry(v.unwrap()).or_insert_with(Vec::new).push(edge_label);
    }
}

fn find_roots_dfs(args: &mut Args, g: &HashMap<String, HashSet<String>>, ga: &mut GraphAttribs, num_known: &HashMap<String, u32>, roots: &HashMap<String, String>, x: &str) {
    let mut g = g.clone();
    if args.weak_maps || args.weak_maps_maps_live {
        pretend_about_weak_maps(args, &mut g, ga);
    }

    let revg = reverse_graph(&g);
    let mut visited = HashSet::new();
    let mut rev_path = Vec::new();
    let mut any_found = false;

    fn find_roots_inner(args: &mut Args, ga: &GraphAttribs, num_known: &HashMap<String, u32>, roots: &HashMap<String, String>, revg: &HashMap<String, HashSet<String>>, visited: &mut HashSet<String>, rev_path: &mut Vec<String>, any_found: &mut bool, y: &str, x: &str) -> bool {
        if visited.contains(y) {
            return false;
        }
        visited.insert(y.to_string());

        if roots.contains_key(y) {
            let known_edges_fn = |node: &str| reverse_graph_known_edges(revg, node);
            let mut path = rev_path.clone();
            path.reverse();
            path.push(x.to_string());
            print_path(args, &known_edges_fn, ga, num_known, roots, &path);
            *any_found = true;
        } else {
            if !revg.contains_key(y) {
                return false;
            }
            rev_path.push("".to_string());
            for z in &revg[y] {
                let last = rev_path.len() - 1;
                rev_path[last] = z.clone();
                if find_roots_inner(args, ga, num_known, roots, revg, visited, rev_path, any_found, z, x) {
                    return true;
                }
            }
            rev_path.pop();
        }
        false
    }

    if !revg.contains_key(x) && !roots.contains_key(x) {
        println!("No other nodes point to {} and it is not a root.\n", x);
        return;
    }

    find_roots_inner(args, ga, num_known, roots, &revg, &mut visited, &mut rev_path, &mut any_found, x, x);

    if !any_found && !args.print_roots_only {
        println!("No roots found for {}", x);
        let known_edges = reverse_graph_known_edges(&revg, x);
        print_known_edges(args, &known_edges, ga, x);
    }
}

fn select_roots(args: &Args, g: &HashMap<String, HashSet<String>>, ga: &GraphAttribs, res: &(HashMap<String, u32>, HashSet<String>)) -> HashMap<String, String> {
    let mut roots = HashMap::new();

    for x in g.keys() {
        if !args.ignore_rc_roots && (res.0.contains_key(x) || ga.incr_roots.contains(x)) {
            roots.insert(x.clone(), "rcRoot".to_string());
        } else if !args.ignore_js_roots && (ga.gc_nodes.get(x).map(|&b| b).unwrap_or(false) || ga.incr_roots.contains(x)) {
            roots.insert(x.clone(), "gcRoot".to_string());
        } else if let Some(node_roots) = &args.node_roots {
            if ga.node_labels.get(x).map(|s| s.as_str()) == Some(node_roots) {
                roots.insert(x.clone(), "stopNodeLabel".to_string());
            }
        }
    }

    roots
}

fn select_targets(g: &HashMap<String, HashSet<String>>, ga: &GraphAttribs, target: &str) -> Vec<String> {
    let addr_patt = Regex::new(r"[A-F0-9]+$|0x[a-f0-9]+$").unwrap();
    if addr_patt.is_match(target) {
        return vec![target.to_string()];
    }

    let mut targs = Vec::new();

    if target == "nsFrameLoader1" {
        let target = "nsFrameLoader";
        for x in g.keys() {
            if ga.node_labels.get(x).map(|s| s.starts_with(target)).unwrap_or(false) && ga.rc_nodes.get(x).map(|&rc| rc == 1).unwrap_or(false) {
                targs.push(x.clone());
            }
        }
        if targs.is_empty() {
            println!("Didn't find any nsFrameLoaders with refcount of 1");
            std::process::exit(-1);
        }
        return targs;
    }

    for x in g.keys() {
        if ga.node_labels.get(x).map(|s| s.starts_with(target)).unwrap_or(false) {
            targs.push(x.clone());
        }
    }

    if targs.is_empty() {
        println!("Didn't find any targets.");
    }

    targs
}

pub fn find_cc_roots(buf: &[u8], target: String) -> String {
    println!("Parsing cycle collector graph...");
    println!("Fod");
    let mut args = Args::default();
    args.file_name = "cc-edges.11270.1736388050.log".to_string();
    args.target = "JSWindowActorProtocol".to_string();
    let output = Vec::new();
    let output_file = Box::new(std::io::Cursor::new(output));
    args.output_file = output_file;

    let (g, mut ga, res) = parse_cc_edge_buf(buf).unwrap();
    let desc = format!("{} nodes, {} edges, {} weak map entries", g.len(), ga.edge_labels.len(), ga.weak_map_entries.len());
    let g = to_single_graph(g);

    let roots = select_roots(&args, &g, &ga, &res);
    let targs = select_targets(&g, &ga, &args.target);

    for a in targs {
        if g.contains_key(&a) {
            if args.use_dfs {
                find_roots_dfs(&mut args, &g, &mut ga, &res.0, &roots, &a);
            } else {
                println!();
                find_roots_bfs(&mut args, &g, &mut ga, &res.0, &roots, &a);
            }
        } else {
            write!(args.output_file, "{} is not in the graph.", a);
        }
    }

    write!(args.output_file, " is not in the graph.");


    if args.output_to_file {
        args.output_file.flush().unwrap();
    }
    args.output_file.flush().unwrap();

/* 
    let mut output_file = Box::new(std::io::Cursor::new(Vec::new()));
    write!(output_file, "dancing");
    output_file.flush();

    //desc + a
    */
    OUTPUT.lock().unwrap().clone()
}
