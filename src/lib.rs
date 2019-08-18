#![allow(dead_code)]
#![allow(unused_imports)]

use std::fmt::Debug;
use std::collections::HashMap;
use itertools::Itertools;

#[derive(Hash, Eq, PartialEq)]
struct Competitor {
  name: String
}

impl Competitor {
  fn from(name: &str) -> Competitor {
    Competitor {
      name: String::from(name)
    }
  }
}

#[derive(Hash, Eq, PartialEq)]
struct Judge {
  name: String,
  head: bool
}

enum ResultFactor {
  ClearMajority { ordinal_favored: u8, ordinal_unfavored: u8 },
  SumTotal { majority_ordinal: u8, sum_favored: u8, sum_unfavored: u8 },
  NextScore { majority_ordinal: u8, diff_ordinal: u8, score_favored: u8, score_unfavored: u8 },
  Battle { judged_favored: u8, judged_unfavored: u8 },
  TieBroken
}

enum TabulationError {
  ZeroSize,
  NoHeadJudge,

}

struct ResultComparison<'a> {
  favored: &'a Competitor,
  unfavored: &'a Competitor,
  factor: ResultFactor
}

struct Tabulation<'a> {
  placements: Vec<&'a Competitor>,
  comparisons: Vec<ResultComparison<'a>>
}

enum TabulationStepOutcome<'a> {
  Win { place: u8, who: &'a Competitor },
  Tie { place: u8, who: Vec<&'a CompetitorOrdinals<'a>> }
}

struct TabulationStepResult<'a> {
  kind: TabulationStepOutcome<'a>,
  products: Vec<ResultComparison<'a>>
}

impl<'a> Tabulation<'a> {

  fn clear_majority_step(place: u8, tied: Vec<&'a CompetitorOrdinals<'a>>) -> Vec<TabulationStepResult<'a>> {
    let sorted_groups: Vec<(u8, Vec<&CompetitorOrdinals>)> = tied
      .iter()
      .group_by(|scores| scores.majority_reached_ordinal())
      .into_iter()
      .sorted_by(|(key, _), (key_other, _)| key.cmp(key_other))
      .map(|(ordinal, group)| (ordinal, group.cloned().collect()) )
      .collect();
    
    let mut offset = 0u8;
    let mut traversed: Vec<(u8, Vec<&CompetitorOrdinals>)> = vec![];
    sorted_groups.iter().map(|(ordinal, group)| {
      let kind = match group.len() {
        0 => panic!("Unexpected zero size group"),
        1 => TabulationStepOutcome::Win { place: place + offset, who: group[0].competitor },
        _ => TabulationStepOutcome::Tie { place: place + offset, who: group.clone() }
      };
      let products = {
        let mut comparisons: Vec<ResultComparison> = vec![];
        traversed.iter().for_each(|(t_ordinal, t_group)| {
          t_group.iter().for_each(|t_competitor_scores| {
            group.iter().for_each(|competitor_scores| {
              comparisons.push(ResultComparison {
                favored: t_competitor_scores.competitor,
                unfavored: competitor_scores.competitor,
                factor: ResultFactor::ClearMajority {
                  ordinal_favored: *t_ordinal,
                  ordinal_unfavored: *ordinal
                }
              });
            });
          });
        });
        comparisons
      };
      
      traversed.push((*ordinal, group.clone()));
      offset += group.len() as u8;

      TabulationStepResult {
        kind,
        products
      }
    }).collect()
  }

  fn sum_total_step(place: u8, tied: Vec<&'a CompetitorOrdinals<'a>>) -> Vec<TabulationStepResult<'a>> {
    let sorted_groups: Vec<(u8, Vec<&CompetitorOrdinals>)> = tied
      .iter()
      .group_by(|scores| scores.sum_below_or_equal(scores.majority_reached_ordinal()))
      .into_iter()
      .sorted_by(|(key, _), (key_other, _)| key.cmp(key_other))
      .map(|(ordinal, group)| (ordinal, group.cloned().collect()) )
      .collect();

    let mut offset = 0u8;
    let mut traversed: Vec<(u8, Vec<&CompetitorOrdinals>)> = vec![];
    sorted_groups.iter().map(|(ordinal, group)| {
      let kind = match group.len() {
        0 => panic!("Unexpected zero size group"),
        1 => TabulationStepOutcome::Win { place: place + offset, who: group[0].competitor },
        _ => TabulationStepOutcome::Tie { place: place + offset, who: group.clone() }
      };
      let products = {
        let mut comparisons: Vec<ResultComparison> = vec![];
        traversed.iter().for_each(|(t_ordinal, t_group)| {
          t_group.iter().for_each(|t_competitor_scores| {
            group.iter().for_each(|competitor_scores| {
              comparisons.push(ResultComparison {
                favored: t_competitor_scores.competitor,
                unfavored: competitor_scores.competitor,
                factor: ResultFactor::ClearMajority {
                  ordinal_favored: *t_ordinal,
                  ordinal_unfavored: *ordinal
                }
              });
            });
          });
        });
        comparisons
      };
      
      traversed.push((*ordinal, group.clone()));
      offset += group.len() as u8;

      TabulationStepResult {
        kind,
        products
      }
    }).collect()
  }

  fn from(info: Vec<&CompetitorOrdinals<'a>>) -> Result<Tabulation<'a>, TabulationError> {

    // let mut placements: Vec<&Competitor> = vec![];
    // let mut comparisons: Vec<ResultComparison> = vec![];
    let place = 1u8;

    let clear_majority_step_results = Tabulation::clear_majority_step(place, info.clone());

    // let sum_total_step_results = clear_majority_step_results.iter().filter_map(|result| {
    //   match result.kind {
    //     TabulationStepOutcome::Win{ place: _, who: _} => None,
    //     TabulationStepOutcome::Tie{ place: tied_place, who: tied} => {
    //       Tabulation::sum_total_step(tied_place, tied)
    //     }
    //   }
    // });

    
    Err(TabulationError::ZeroSize)
  }
}

struct CompetitorOrdinals<'a> {
  competitor: &'a Competitor,
  ordinal_scores: HashMap<&'a Judge, u8>
}

impl CompetitorOrdinals<'_> {
  fn below_or_equal(&self, value: u8) -> impl Iterator<Item = &u8> {
    self.ordinal_scores
      .values()
      .filter(move |&&ordinal| ordinal <= value)
  }

  fn ordinals_cloned(&self) -> Vec<u8> {
    self.ordinal_scores
      .values()
      .cloned()
      .collect()
  }

  fn sorted_ordinals(&self) -> Vec<u8> {
    let mut ordinals_clone = self.ordinals_cloned();
    ordinals_clone.sort();
    ordinals_clone
  }

  fn majority_reached_ordinal(&self) -> u8 {
    let majority_index = (self.ordinal_scores.len() as f64 / 2f64)
      .floor() as usize;
    self.sorted_ordinals()[majority_index]
  }

  fn sum_below_or_equal(&self, value: u8) -> u8 {
    self.below_or_equal(value).sum()
  }

  fn count_below_or_equal(&self, value: u8) -> usize {
    self.below_or_equal(value).count()
  }
}


#[cfg(test)]
mod tests {
  use super::*;
  use std::iter;
  use rand::{thread_rng, Rng};
  use rand::distributions::Alphanumeric;

  macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
      {
        let mut m = HashMap::new();
        $(
          m.insert($key, $value);
        )+
        m
      }
    };
  );

  fn generate_random_judges(number: usize) -> Vec<Judge> {
    vec![Judge::random(true)]
      .into_iter()
      .chain(iter::repeat_with(|| Judge::random(false)))
      .take(number)
      .collect()
  }

  fn generate_random_name(length: usize) -> String {
    thread_rng()
      .sample_iter(&Alphanumeric)
      .take(length)
      .collect()
  }

  impl Competitor {
    fn random() -> Competitor {
      Competitor {
        name: generate_random_name(8)
      }
    }
  }

  impl Judge {
    fn random(head: bool) -> Judge {
      Judge {
        name: generate_random_name(8),
        head
      }
    }
  }

  #[test]
  fn count_below_or_equal() {
    let judges = generate_random_judges(4);
    let competitor = Competitor::random();
    let ordinals = CompetitorOrdinals {
      competitor: &competitor,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 1,
        &judges[2] => 3,
        &judges[3] => 5
      )
    };
    let count = |value| ordinals.count_below_or_equal(value);

    assert_eq!(count(1), 2);
    assert_eq!(count(2), 2);
    assert_eq!(count(3), 3);
    assert_eq!(count(4), 3);
    assert_eq!(count(5), 4);
  }

  #[test]
  fn sum_below_or_equal() {
    let judges = generate_random_judges(4);
    let competitor = Competitor::random();
    let ordinals = CompetitorOrdinals {
      competitor: &competitor,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 1,
        &judges[2] => 3,
        &judges[3] => 5
      )
    };
    let sum = |value| ordinals.sum_below_or_equal(value);

    assert_eq!(sum(1), 2);
    assert_eq!(sum(2), 2);
    assert_eq!(sum(3), 5);
    assert_eq!(sum(4), 5);
    assert_eq!(sum(5), 10);
  }

  #[test]
  fn majority_reached_ordinal_even() {
    let judges = generate_random_judges(4);
    let competitor = Competitor::random();
    let ordinals = CompetitorOrdinals {
      competitor: &competitor,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 1,
        &judges[2] => 3,
        &judges[3] => 5
      )
    };

    assert_eq!(ordinals.majority_reached_ordinal(), 3);
  }

  #[test]
  fn majority_reached_ordinal_odd() {
    let judges = generate_random_judges(5);
    let competitor = Competitor::random();
    let ordinals = CompetitorOrdinals {
      competitor: &competitor,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 2,
        &judges[2] => 2,
        &judges[3] => 3,
        &judges[4] => 4
      )
    };

    assert_eq!(ordinals.majority_reached_ordinal(), 2);
  }

  #[test]
  fn clear_majority_step_win() {
    let judges = generate_random_judges(4);
    let competitor_0 = Competitor::random();
    let ordinals_0 = CompetitorOrdinals {
      competitor: &competitor_0,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 1,
        &judges[2] => 1,
        &judges[3] => 2
      )
    };
    let competitor_1 = Competitor::random();
    let ordinals_1 = CompetitorOrdinals {
      competitor: &competitor_1,
      ordinal_scores: map!(
        &judges[0] => 2,
        &judges[1] => 2,
        &judges[2] => 2,
        &judges[3] => 1
      )
    };
    let step_results = Tabulation::clear_majority_step(1u8, vec![&ordinals_0, &ordinals_1]);
    
    assert!(match &step_results[0].kind {
      TabulationStepOutcome::Win{ place: 1u8, who } => {
        **who == competitor_0
      },
      _ => false
    });
    assert!(match &step_results[1].kind {
      TabulationStepOutcome::Win{ place: 2u8, who } => {
        **who == competitor_1
      },
      _ => false
    });
  }

  #[test]
  fn clear_majority_step_tie() {
    let judges = generate_random_judges(4);
    let competitor_0 = Competitor::random();
    let ordinals_0 = CompetitorOrdinals {
      competitor: &competitor_0,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 1,
        &judges[2] => 2,
        &judges[3] => 2
      )
    };
    let competitor_1 = Competitor::random();
    let ordinals_1 = CompetitorOrdinals {
      competitor: &competitor_1,
      ordinal_scores: map!(
        &judges[0] => 2,
        &judges[1] => 2,
        &judges[2] => 1,
        &judges[3] => 1
      )
    };
    let step_results = Tabulation::clear_majority_step(1u8, vec![&ordinals_0, &ordinals_1]);
    
    assert!(match &step_results[0].kind {
      TabulationStepOutcome::Tie{ place: 1u8, who } => {
        who.len() == 2 &&
        who.iter().any(|scores| *scores.competitor == competitor_0) &&
        who.iter().any(|scores| *scores.competitor == competitor_1)
      },
      _ => false
    });
  }
}