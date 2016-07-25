


combine_moments = function(m1, m2) {

  var ci_width, delta1, m1_M1, m1_M2, m1_n, m1_na_count, m2_M1, m2_M2, m2_n, m2_na_count, n, order, res, t_val, part;

  m1_M1 = m1.M1[0], m1_M2 = m1.M2[0], m1_n = m1.n[0], m1_na_count = m1.na_count[0];
  m2_M1 = m2.M1[0], m2_M2 = m2.M2[0], m2_n = m2.n[0], m2_na_count = m2.na_count[0];

  if (m1_n < 1) {
    // to avoid pointer issues
    return {
      M1: [m2_M1],
      M2: [m2_M2],
      n: [m2_n],
      na_count: [m1_na_count + m2_na_count],
      mean: [m2.mean],
      sd: [m2.sd],
      ci_lower: [m2.ci_lower],
      ci_upper: [m2.ci_upper]
    };
  }
  if (m2_n < 1) {
    // to avoid pointer issues
    return {
      M1: [m1_M1],
      M2: [m1_M2],
      n: [m1_n],
      na_count: [m1_na_count + m2_na_count],
      mean: m1.mean,
      sd: m1.sd,
      ci_lower: m1.ci_lower,
      ci_upper: m1.ci_upper
    };
  }

  order = 2;
  n = m1_n + m2_n;
  delta1 = m2_M1 - m1_M1;
  part = (m1_n * m2_n * delta1 / n);

  res = {
    M1: m1_M1 + (m2_n * delta1 / n),
    M2: m1_M2 + m2_M2 + (1 / m2_n + 1 / m1_n) * part * part,
    n: n,
    na_count: m1_na_count + m2_na_count
  };

  res.mean = res.M1;
  res.sd = Math.sqrt(res.M2 / (n - 1));

  t_val = jStat_studentt_inv(0.975, n);
  ci_width = res.sd * t_val / Math.sqrt(n);

  res.ci_lower = res.mean - ci_width;
  res.ci_upper = res.mean + ci_width;

  return {
    M1: [res.M1],
    M2: [res.M2],
    n: [res.n],
    na_count: [res.na_count],
    mean: [res.mean],
    sd: [res.sd],
    ci_lower: [res.ci_lower],
    ci_upper: [res.ci_upper]
  };
};



combine_moments_over_time = function(obj1_time_bins, obj2_time_bins) {
  var obj1_keys, obj2_keys, res, unique_time_keys, i, key_i;

  obj1_keys = Object.keys(obj1_time_bins)
  obj2_keys = Object.keys(obj2_time_bins)

  unique_time_keys = Array.from(new Set(obj1_keys.concat(obj2_keys)));
  unique_time_keys_len = unique_time_keys.length

  res = {}
  for (i = 0; i < unique_time_keys_len; i++) {
    key_i = unique_time_keys[i]
    if (obj1_time_bins.hasOwnProperty(key_i)) {
      if (obj2_time_bins.hasOwnProperty(key_i)) {
        // both have the key, merge them
        res[key_i] = combine_moments(
          obj1_time_bins[key_i],
          obj2_time_bins[key_i]
        )
      } else {
        // only obj 1 has it
        res[key_i] = obj1_time_bins[key_i]
      }
    } else {
      // only obj 2 has it
      res[key_i] = obj2_time_bins[key_i]
    }
  }

  return res
}
