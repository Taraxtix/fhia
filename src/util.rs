use std::collections::{HashMap, VecDeque};

pub fn topo_order<'a>(
    dep_map: &HashMap<&'a str, Vec<&'a str>>,
) -> Result<Vec<&'a str>, Vec<&'a str>> {
    let mut in_deg: HashMap<&'a str, usize> = dep_map.iter().map(|(&n, d)| (n, d.len())).collect();

    let mut rdeps: HashMap<&'a str, Vec<&'a str>> = HashMap::new();
    for (&name, deps) in dep_map
    {
        for &dep in deps
        {
            rdeps.entry(dep).or_default().push(name);
        }
    }

    let mut queue: VecDeque<&'a str> = {
        let mut v: Vec<&'a str> = in_deg
            .iter()
            .filter_map(|(&n, &d)| (d == 0).then_some(n))
            .collect();
        v.sort_unstable();
        v.into()
    };

    let mut order = Vec::with_capacity(dep_map.len());
    while let Some(name) = queue.pop_front()
    {
        order.push(name);
        let mut newly_freed: Vec<&'a str> = rdeps
            .get(name)
            .map_or([].as_slice(), Vec::as_slice)
            .iter()
            .filter_map(|&dep| {
                let d = in_deg.get_mut(dep).unwrap();
                *d -= 1;
                (*d == 0).then_some(dep)
            })
            .collect();
        newly_freed.sort_unstable();
        queue.extend(newly_freed);
    }

    if order.len() == dep_map.len()
    {
        Ok(order)
    }
    else
    {
        let cycle = in_deg
            .into_iter()
            .filter_map(|(name, deg)| (deg > 0).then_some(name))
            .collect();
        Err(cycle)
    }
}
