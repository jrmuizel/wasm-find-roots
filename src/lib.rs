use wasm_bindgen::prelude::*;
mod find_roots;
mod parse_cc_graph;
use std::panic;


#[wasm_bindgen]
pub fn count_lines(file_data: &[u8], target: String) -> String {
    panic::set_hook(Box::new(console_error_panic_hook::hook));

    //file_data.iter().filter(|&&byte| byte == b'\n').count() + 1
    //let (g, mut ga, res) = parse_cc_graph::parse_cc_edge_buf(file_data).unwrap();
    
    //g.len()
    find_roots::find_cc_roots(file_data, target)
    //"Hello, World!".to_string()
}
