// SPDX-License-Identifier: MPL-2.0
//! Deterministic, thresholded spelling candidates. Identifiers retain the
//! language's current exact Unicode-scalar identity (no implicit normalization).
pub fn nearby(name: &str, mut candidates: Vec<(String, usize, usize)>) -> Vec<String> {
    let length = name.chars().count();
    candidates.retain(|(candidate, _, _)| candidate.chars().count().abs_diff(length) <= 2);
    candidates.sort_by(|a, b| (a.1, a.2, &a.0).cmp(&(b.1, b.2, &b.0)));
    let mut seen = std::collections::HashSet::new();
    candidates.retain(|candidate| seen.insert(candidate.0.clone()));
    let mut matches = candidates
        .into_iter()
        .take(256)
        .filter_map(|(candidate, scope, order)| {
            distance(name, &candidate).map(|distance| (distance, scope, order, candidate))
        })
        .collect::<Vec<_>>();
    matches.sort();
    matches.into_iter().take(3).map(|(_, _, _, name)| name).collect()
}

fn distance(a: &str, b: &str) -> Option<usize> {
    let a = a.chars().collect::<Vec<_>>();
    let b = b.chars().collect::<Vec<_>>();
    if a.len().abs_diff(b.len()) > 2 {
        return None;
    }
    let mut previous = vec![3; b.len() + 1];
    for (j, cell) in previous.iter_mut().enumerate().take(3) {
        *cell = j;
    }
    let mut current = vec![3; b.len() + 1];
    for (i, c) in a.iter().enumerate() {
        let row = i + 1;
        let low = row.saturating_sub(2).max(1);
        let high = (row + 2).min(b.len());
        current[low - 1] = 3;
        if high < b.len() {
            current[high + 1] = 3;
        }
        current[0] = row.min(3);
        let mut best = current[0];
        for j in low..=high {
            current[j] = (previous[j] + 1)
                .min(current[j - 1] + 1)
                .min(previous[j - 1] + usize::from(*c != b[j - 1]));
            best = best.min(current[j]);
        }
        if best > 2 {
            return None;
        }
        std::mem::swap(&mut previous, &mut current);
    }
    (previous[b.len()] <= 2).then_some(previous[b.len()])
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ranking_is_bounded_deterministic_and_unicode_aware() {
        assert_eq!(
            nearby("coutn", vec![("count".into(), 0, 1), ("court".into(), 1, 0)]),
            ["count", "court"]
        );
        assert_eq!(distance("변수", "변수값"), Some(1));
        assert_eq!(distance("abc", "xyz"), None);
        assert_eq!(distance("", "a"), Some(1));
        let candidates = (0..300).map(|n| (format!("aaaa{n:03}"), 0, n)).collect::<Vec<_>>();
        assert!(!nearby("aaaa299", candidates).contains(&"aaaa299".into()));
    }
}

#[cfg(test)]
mod band_tests {
    use super::*;

    #[test]
    fn threshold_matches_exhaustive_short_word_distances() {
        let mut words = vec![String::new()];
        for length in 1..=5 {
            for bits in 0usize..(1 << length) {
                words.push(
                    (0..length).map(|i| if bits & (1 << i) == 0 { 'a' } else { 'b' }).collect(),
                );
            }
        }
        for a in &words {
            for b in &words {
                let mut matrix = vec![vec![0; b.len() + 1]; a.len() + 1];
                for (i, row) in matrix.iter_mut().enumerate() {
                    row[0] = i;
                }
                for (j, cell) in matrix[0].iter_mut().enumerate() {
                    *cell = j;
                }
                for (i, x) in a.bytes().enumerate() {
                    for (j, y) in b.bytes().enumerate() {
                        matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                            .min(matrix[i + 1][j] + 1)
                            .min(matrix[i][j] + usize::from(x != y));
                    }
                }
                let expected = matrix[a.len()][b.len()];
                assert_eq!(distance(a, b), (expected <= 2).then_some(expected), "{a:?} / {b:?}");
            }
        }
        let long = "a".repeat(100_000);
        assert_eq!(distance(&long, &long), Some(0));
        assert_eq!(
            nearby(
                "coutn",
                vec![("count".into(), 0, 0), ("court".into(), 0, 1), ("count".into(), 1, 0)]
            ),
            ["count", "court"]
        );
    }
}
