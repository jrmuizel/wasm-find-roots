use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;
use regex::Regex;

const file_has_counts: bool = false;

#[derive(Debug)]
pub struct GraphAttribs {
    pub edge_labels: HashMap<String, HashMap<String, Vec<String>>>,
    pub node_labels: HashMap<String, String>,
    pub rc_nodes: HashMap<String, u32>,
    pub gc_nodes: HashMap<String, bool>,
    pub xpc_roots: HashSet<String>,
    pub purp_roots: HashSet<String>,
    pub weak_map_entries: Vec<WeakMapEntry>,
    pub incr_roots: HashSet<String>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct WeakMapEntry {
    pub weak_map: String,
    pub key: String,
    pub key_delegate: String,
    pub value: String,
}

fn parse_graph(f: &mut impl BufRead, root_counts: (usize, usize)) -> (HashMap<String, HashMap<String, u32>>, GraphAttribs) {
    let mut edges: HashMap<String, HashMap<String, u32>> = HashMap::new();
    let mut edge_labels: HashMap<String, HashMap<String, Vec<String>>> = HashMap::new();
    let mut node_labels: HashMap<String, String> = HashMap::new();
    let mut rc_nodes: HashMap<String, u32> = HashMap::new();
    let mut gc_nodes: HashMap<String, bool> = HashMap::new();
    let mut weak_map_entries: Vec<WeakMapEntry> = Vec::new();
    let mut incr_roots: HashSet<String> = HashSet::new();
    let mut xpc_roots: HashSet<String> = HashSet::new();
    let mut purp_roots: HashSet<String> = HashSet::new();
    let mut num_nodes = 0;

    let node_patt = Regex::new(r"([a-zA-Z0-9]+) \[(rc=[0-9]+|gc(?:.marked)?)\] ([^\r\n]*)\r?$").unwrap();
    let edge_patt = Regex::new(r"> ([a-zA-Z0-9]+) ([^\r\n]*)\r?$").unwrap();
    let weak_map_entry_patt = Regex::new(r"WeakMapEntry map=([a-zA-Z0-9]+|\(nil\)) key=([a-zA-Z0-9]+|\(nil\)) keyDelegate=([a-zA-Z0-9]+|\(nil\)) value=([a-zA-Z0-9]+)\r?$").unwrap();
    let incr_root_patt = Regex::new(r"IncrementalRoot ([a-zA-Z0-9]+)\r?$").unwrap();

    let mut curr_node: Option<String> = None;

    for line in f.lines() {
        let line = line.unwrap();
        if line.starts_with('>') {
            let e = edge_patt.captures(&line).unwrap();
            let target = e.get(1).unwrap().as_str().to_string();
            let edge_label = e.get(2).unwrap().as_str().to_string();

            let node = curr_node.as_ref().unwrap();
            *edges.entry(node.clone()).or_default().entry(target.clone()).or_default() += 1;
            if !edge_label.is_empty() {
                edge_labels.entry(node.clone()).or_default().entry(target).or_default().push(edge_label);
            }
        } else {
            if let Some(nm) = node_patt.captures(&line) {
                curr_node = Some(nm.get(1).unwrap().as_str().to_string());
                num_nodes += 1;
                if num_nodes <= root_counts.0 {
                    xpc_roots.insert(curr_node.clone().unwrap());
                } else if num_nodes <= root_counts.1 {
                    purp_roots.insert(curr_node.clone().unwrap());
                }
                let node_ty = nm.get(2).unwrap().as_str();
                let is_ref_counted = !node_ty.starts_with("gc");
                let node_info = if node_ty == "gc" {
                    0
                } else if node_ty == "gc.marked" {
                    1
                } else {
                    node_ty[3..].parse().unwrap()
                };
                let node_label = nm.get(3).unwrap().as_str().to_string();
                add_node(&mut edges, &mut edge_labels, &mut node_labels, &mut rc_nodes, &mut gc_nodes, &curr_node.clone().unwrap(), is_ref_counted, node_info, node_label);
            } else if line.starts_with("==========") {
                break;
            } else if let Some(wmem) = weak_map_entry_patt.captures(&line) {
                weak_map_entries.push(WeakMapEntry {
                    weak_map: wmem.get(1).unwrap().as_str().to_string(),
                    key: wmem.get(2).unwrap().as_str().to_string(),
                    key_delegate: wmem.get(3).unwrap().as_str().to_string(),
                    value: wmem.get(4).unwrap().as_str().to_string(),
                });
            } else if let Some(iroot) = incr_root_patt.captures(&line) {
                incr_roots.insert(iroot.get(1).unwrap().as_str().to_string());
            } else if !line.starts_with('#') {
                eprintln!("Error: skipping unknown line: {}", line);
            }
        }
    }

    let ga = GraphAttribs {
        edge_labels,
        node_labels,
        rc_nodes,
        gc_nodes,
        xpc_roots,
        purp_roots,
        weak_map_entries,
        incr_roots,
    };

    (edges, ga)
}

fn add_node(
    edges: &mut HashMap<String, HashMap<String, u32>>,
    edge_labels: &mut HashMap<String, HashMap<String, Vec<String>>>,
    node_labels: &mut HashMap<String, String>,
    rc_nodes: &mut HashMap<String, u32>,
    gc_nodes: &mut HashMap<String, bool>,
    node: &str,
    is_ref_counted: bool,
    node_info: u32,
    node_label: String,
) {
    edges.insert(node.to_string(), HashMap::new());
    edge_labels.insert(node.to_string(), HashMap::new());
    if is_ref_counted {
        rc_nodes.insert(node.to_string(), node_info);
    } else {
        gc_nodes.insert(node.to_string(), node_info != 0);
    }
    if !node_label.is_empty() {
        node_labels.insert(node.to_string(), node_label);
    }
}

fn parse_results(f: &mut impl BufRead) -> (HashMap<String, u32>, HashSet<String>) {
    let result_patt = Regex::new(r"([a-zA-Z0-9]+) \[([a-z0-9=]+)\]\w*").unwrap();
    let known_patt = Regex::new(r"known=(\d+)").unwrap();

    let mut garbage = HashSet::new();
    let mut known_edges = HashMap::new();

    for line in f.lines() {
        let line = line.unwrap();
        if let Some(rm) = result_patt.captures(&line) {
            let obj = rm.get(1).unwrap().as_str().to_string();
            let tag = rm.get(2).unwrap().as_str();
            if tag == "garbage" {
                garbage.insert(obj);
            } else if let Some(km) = known_patt.captures(tag) {
                known_edges.insert(obj, km.get(1).unwrap().as_str().parse().unwrap());
            } else {
                eprintln!("Error: Unknown result entry type: {}", tag);
            }
        } else {
            eprintln!("Error: Unknown result entry: {}", line);
        }
    }

    (known_edges, garbage)
}

fn parse_counts(f: &mut impl BufRead) -> (usize, usize) {
    let count_patt = Regex::new(r"0x0 \[rc=([0-9]+)\] COUNT_ROOTS\r?$").unwrap();
    let line = f.lines().next().unwrap().unwrap();
    let cpm = count_patt.captures(&line).unwrap();
    let xpc_count = cpm.get(1).unwrap().as_str().parse().unwrap();
    let line = f.lines().next().unwrap().unwrap();
    let cpm = count_patt.captures(&line).unwrap();
    let purple_count = cpm.get(1).unwrap().as_str().parse().unwrap();
    (xpc_count, purple_count)
}

pub fn parse_cc_edge_file(fname: &str) -> Result<(HashMap<String, HashMap<String, u32>>, GraphAttribs, (HashMap<String, u32>, HashSet<String>)), io::Error> {
    let mut f = BufReader::new(File::open(fname)?);
    let root_counts = if file_has_counts { parse_counts(&mut f) } else { (0, 0) };
    let (edges, ga) = parse_graph(&mut f, root_counts);
    let results = parse_results(&mut f);
    Ok((edges, ga, results))
}

pub fn parse_cc_edge_buf(buf: &[u8]) -> Result<(HashMap<String, HashMap<String, u32>>, GraphAttribs, (HashMap<String, u32>, HashSet<String>)), io::Error> {
    let mut f = BufReader::new(buf);
    let root_counts = if file_has_counts { parse_counts(&mut f) } else { (0, 0) };
    let (edges, ga) = parse_graph(&mut f, root_counts);
    let results = parse_results(&mut f);
    Ok((edges, ga, results))
}

pub fn to_single_graph(gm: HashMap<String, HashMap<String, u32>>) -> HashMap<String, HashSet<String>> {
    let mut g = HashMap::new();
    for (src, dsts) in gm {
        let d: HashSet<String> = dsts.keys().cloned().collect();
        g.insert(src, d);
    }
    g
}

fn reverse_multigraph(gm: HashMap<String, HashMap<String, u32>>) -> HashMap<String, HashMap<String, u32>> {
    let mut gm2 = HashMap::new();
    for (src, dsts) in gm {
        for (dst, k) in dsts {
            gm2.entry(dst).or_insert_with(HashMap::new).insert(src.clone(), k);
        }
    }
    gm2
}

fn print_graph(g: &HashMap<String, HashMap<String, u32>>) {
    println!("Graph:");
    for (x, edges) in g {
        print!("  {}: ", x);
        for (e, k) in edges {
            for _ in 0..*k {
                print!("{}, ", e);
            }
        }
        println!();
    }
}

fn print_attribs(ga: &GraphAttribs) {
    print!("RC nodes: ");
    for (x, rc) in &ga.rc_nodes {
        print!("{}={}, ", x, rc);
    }
    println!();

    print!("Marked GC nodes: ");
    for (x, marked) in &ga.gc_nodes {
        if *marked {
            print!("{}, ", x);
        }
    }
    println!();

    print!("Unmarked GC nodes: ");
    for (x, marked) in &ga.gc_nodes {
        if !*marked {
            print!("{}, ", x);
        }
    }
    println!();

    print!("Node labels: ");
    for (x, l) in &ga.node_labels {
        print!("{}:{}, ", x, l);
    }
    println!();

    print!("Edge labels: ");
    for (src, edges) in &ga.edge_labels {
        for (dst, l) in edges {
            print!("{}->{}:{}, ", src, dst, l.join(", "));
        }
    }
    println!();
}

fn print_results(r: &(HashMap<String, u32>, HashSet<String>)) {
    print!("Known edges: ");
    for (x, k) in &r.0 {
        print!("{}={}, ", x, k);
    }
    println!();

    print!("Garbage: ");
    for x in &r.1 {
        print!("{}, ", x);
    }
    println!();
}

pub fn parse_file_cmd() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Not enough arguments.");
        std::process::exit(1);
    }

    let result = parse_cc_edge_file(&args[1]);
    match result {
        Ok((edges, ga, results)) => {
            print_graph(&edges);
            print_attribs(&ga);
            print_results(&results);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}