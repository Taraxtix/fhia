use std::collections::{HashMap, VecDeque};

pub fn topo_order<'a>(dep_map: &HashMap<&'a str, Vec<&'a str>>) -> Vec<&'a str> {
    let mut in_deg: HashMap<&'a str, usize> = dep_map.iter().map(|(&n, d)| (n, d.len())).collect();

    let mut rdeps: HashMap<&'a str, Vec<&'a str>> = HashMap::new();
    for (&name, deps) in dep_map
    {
        for &dep in deps
        {
            rdeps.entry(dep).or_default().push(name);
        }
    }

    let mut queue: VecDeque<&'a str> = in_deg
        .iter()
        .filter_map(|(&n, &d)| (d == 0).then_some(n))
        .collect();

    let mut order = Vec::with_capacity(dep_map.len());
    while let Some(name) = queue.pop_front()
    {
        order.push(name);
        for &dependent in rdeps.get(name).map_or([].as_slice(), Vec::as_slice)
        {
            let d = in_deg.get_mut(dependent).unwrap();
            *d -= 1;
            if *d == 0
            {
                queue.push_back(dependent);
            }
        }
    }

    assert_eq!(
        order.len(),
        dep_map.len(),
        "cycle in top-level declarations"
    );
    order
}
