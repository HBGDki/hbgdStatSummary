my_data = {
  "wtkg": {
    "col_name": ["wtkg"],
    "type": ["time-varying-num"],
    "group_duration": ["week"],
    "time_bins": {
      "1": {
        "M1": [3.4076],
        "M2": [121.0645],
        "n": [500],
        "na_count": [0],
        "mean": [3.4076],
        "sd": [0.4926],
        "ci_lower": [3.3644],
        "ci_upper": [3.4509]
      },
      "18": {
        "M1": [6.6768],
        "M2": [363.0968],
        "n": [460],
        "na_count": [0],
        "mean": [6.6768],
        "sd": [0.8894],
        "ci_lower": [6.5953],
        "ci_upper": [6.7583]
      },
      "53": {
        "M1": [10.1565],
        "M2": [634.1717],
        "n": [428],
        "na_count": [0],
        "mean": [10.1565],
        "sd": [1.2187],
        "ci_lower": [10.0408],
        "ci_upper": [10.2723]
      },
      "98": {
        "M1": [10.1565],
        "M2": [634.1717],
        "n": [428],
        "na_count": [0],
        "mean": [10.1565],
        "sd": [1.2187],
        "ci_lower": [10.0408],
        "ci_upper": [10.2723]
      }
    }
  },
  "htcm": {
    "col_name": ["htcm"],
    "type": ["time-varying-num"],
    "group_duration": ["week"],
    "time_bins": {
      "1": {
        "M1": [50.8441],
        "M2": [3506.998],
        "n": [494],
        "na_count": [6],
        "mean": [50.8441],
        "sd": [2.6671],
        "ci_lower": [50.6084],
        "ci_upper": [51.0799]
      },
      "18": {
        "M1": [null],
        "M2": [null],
        "n": [0],
        "na_count": [460],
        "mean": [null],
        "sd": [null],
        "ci_lower": [null],
        "ci_upper": [null]
      },
      "53": {
        "M1": [75.4541],
        "M2": [4013.3553],
        "n": [425],
        "na_count": [3],
        "mean": [75.4541],
        "sd": [3.0766],
        "ci_lower": [75.1608],
        "ci_upper": [75.7475]
      },
      "99": {
        "M1": [75.4541],
        "M2": [4013.3553],
        "n": [425],
        "na_count": [3],
        "mean": [75.4541],
        "sd": [3.0766],
        "ci_lower": [75.1608],
        "ci_upper": [75.7475]
      }
    }
  }
}




combine_moments(my_data.wtkg.time_bins["1"], my_data.htcm.time_bins["1"])
// > datadr::combineMoments(list(M1 = 3.4076, M2 = 121.0645, n = 500), list(M1 = 50.8441, M2 = 3506.998, n = 494)) %>% as.data.frame()
//         M1       M2   n
// 1 26.98268 562787.7 994
// > qt(0.025, df = 993) / sqrt(994) * 23.8066172 + 26.9826814
// [1] 25.50091
// > qt(0.975, df = 993) / sqrt(994) * 23.8066172 + 26.9826814
// [1] 28.46446


combine_moments_over_time(my_data.wtkg.time_bins, my_data.htcm.time_bins)
// {
//   1:{M1:[26.982681488933597],M2:[562787.7390248991],n:[994],na_count:[6],mean:[26.982681488933597],sd:[23.80661723542971],ci_lower:[25.500908352313484],ci_upper:[28.46445462555371]},
//   18:{M1:[6.6768],M2:[363.0968],n:[460],na_count:[460],mean:[6.6768],sd:[0.8894],ci_lower:[6.5953],ci_upper:[6.7583]},
//   53:{M1:[42.69047420867526],M2:[913886.6328754325],n:[853],na_count:[3],mean:[42.69047420867526],sd:[32.75113575200534],ci_lower:[40.48949167628455],ci_upper:[44.89145674106597]},
//   98:{M1:[10.1565],M2:[634.1717],n:[428],na_count:[0],mean:[10.1565],sd:[1.2187],ci_lower:[10.0408],ci_upper:[10.2723]},
//   99:{M1:[75.4541],M2:[4013.3553],n:[425],na_count:[3],mean:[75.4541],sd:[3.0766],ci_lower:[75.1608],ci_upper:[75.7475]}
// }