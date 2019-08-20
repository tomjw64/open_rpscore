#![allow(dead_code)]
#![allow(unused_imports)]

use std::fmt::Debug;
use std::collections::HashMap;
use itertools::Itertools;

#[derive(Hash, Eq, PartialEq, Debug)]
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

#[derive(Hash, Eq, PartialEq, Debug)]
struct Judge {
  name: String,
  head: bool
}

enum ResultFactor {
  ClearMajority,
  GreaterMajority,
  SumBelow,
  FollowingScore,
  Battle,
  TieBroken
}

enum TabulationError {
  ZeroSize,
  NoHeadJudge
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
  outcomes: Vec<TabulationStepOutcome<'a>>,
  products: Vec<ResultComparison<'a>>
}

impl<'a> Tabulation<'a> {
  fn clear_majority_step(place: u8, tied: Vec<&'a CompetitorOrdinals<'a>>) -> TabulationStepResult<'a> {
    let mut result = TabulationStepResult {
      outcomes: vec![],
      products: vec![]
    };
    let sorted_groups: Vec<(u8, Vec<&CompetitorOrdinals>)> = tied
      .iter()
      .group_by(|scores| scores.majority_reached_ordinal())
      .into_iter()
      .sorted_by(|(key, _), (key_other, _)| key.cmp(key_other))
      .map(|(ordinal, group)| (ordinal, group.cloned().collect()) )
      .collect();
    
    let mut offset = 0u8;
    let mut traversed: Vec<(u8, Vec<&CompetitorOrdinals>)> = vec![];
    sorted_groups.iter().for_each(|(ordinal, group)| {
      result.outcomes.push(match group.len() {
        0 => panic!("Unexpected zero size group"),
        1 => TabulationStepOutcome::Win { place: place + offset, who: group[0].competitor },
        _ => TabulationStepOutcome::Tie { place: place + offset, who: group.clone() }
      });
    
      traversed.iter().for_each(|(_, t_group)| {
        t_group.iter().for_each(|t_competitor_scores| {
          group.iter().for_each(|competitor_scores| {
            result.products.push(ResultComparison {
              favored: t_competitor_scores.competitor,
              unfavored: competitor_scores.competitor,
              factor: ResultFactor::ClearMajority
            });
          });
        });
      });
      
      traversed.push((*ordinal, group.clone()));
      offset += group.len() as u8;
    });

    result
  }

  fn greater_majority_step(place: u8, tied: Vec<&'a CompetitorOrdinals<'a>>) -> TabulationStepResult<'a> {
    let mut result = TabulationStepResult {
      outcomes: vec![],
      products: vec![]
    };
    let sorted_groups: Vec<(u8, Vec<&CompetitorOrdinals>)> = tied
      .iter()
      .group_by(|scores| scores.count_below_or_equal(scores.majority_reached_ordinal()) )
      .into_iter()
      .sorted_by(|(key, _), (key_other, _)| key.cmp(key_other).reverse())
      .map(|(count, group)| (count, group.cloned().collect()) )
      .collect();
    
    let mut offset = 0u8;
    let mut traversed: Vec<(u8, Vec<&CompetitorOrdinals>)> = vec![];
    sorted_groups.iter().for_each(|(count, group)| {
      result.outcomes.push(match group.len() {
        0 => panic!("Unexpected zero size group"),
        1 => TabulationStepOutcome::Win { place: place + offset, who: group[0].competitor },
        _ => TabulationStepOutcome::Tie { place: place + offset, who: group.clone() }
      });
    
      traversed.iter().for_each(|(_, t_group)| {
        t_group.iter().for_each(|t_competitor_scores| {
          group.iter().for_each(|competitor_scores| {
            result.products.push(ResultComparison {
              favored: t_competitor_scores.competitor,
              unfavored: competitor_scores.competitor,
              factor: ResultFactor::GreaterMajority
            });
          });
        });
      });
      
      traversed.push((*count, group.clone()));
      offset += group.len() as u8;
    });
    result
  }

  fn sum_below_step(place: u8, tied: Vec<&'a CompetitorOrdinals<'a>>) -> TabulationStepResult<'a> {
    let mut result = TabulationStepResult {
      outcomes: vec![],
      products: vec![]
    };
    let sorted_groups: Vec<(u8, Vec<&CompetitorOrdinals>)> = tied
      .iter()
      .group_by(|scores| scores.sum_below_or_equal(scores.majority_reached_ordinal()) )
      .into_iter()
      .sorted_by(|(key, _), (key_other, _)| key.cmp(key_other))
      .map(|(sum, group)| (sum, group.cloned().collect()) )
      .collect();
    
    let mut offset = 0u8;
    let mut traversed: Vec<(u8, Vec<&CompetitorOrdinals>)> = vec![];
    sorted_groups.iter().for_each(|(sum, group)| {
      result.outcomes.push(match group.len() {
        0 => panic!("Unexpected zero size group"),
        1 => TabulationStepOutcome::Win { place: place + offset, who: group[0].competitor },
        _ => TabulationStepOutcome::Tie { place: place + offset, who: group.clone() }
      });
    
      traversed.iter().for_each(|(_, t_group)| {
        t_group.iter().for_each(|t_competitor_scores| {
          group.iter().for_each(|competitor_scores| {
            result.products.push(ResultComparison {
              favored: t_competitor_scores.competitor,
              unfavored: competitor_scores.competitor,
              factor: ResultFactor::SumBelow
            });
          });
        });
      });
      
      traversed.push((*sum, group.clone()));
      offset += group.len() as u8;
    });
    result
  }

  // fn from(info: Vec<&CompetitorOrdinals<'a>>) -> Result<Tabulation<'a>, TabulationError> {

  //   let mut placements: Vec<&Competitor> = vec![];
  //   let mut comparisons: Vec<ResultComparison> = vec![];
  //   let place = 1u8;

  //   let clear_majority_step_results = Tabulation::clear_majority_step(place, info.clone());

  //   let sum_total_step_results = clear_majority_step_results.iter().filter_map(|result| {
  //     match result.kind {
  //       TabulationStepOutcome::Win{ place: _, who: _} => None,
  //       TabulationStepOutcome::Tie{ place: tied_place, who: tied} => {
  //         Tabulation::sum_total_step(tied_place, tied)
  //       }
  //     }
  //   });

    
  //   Err(TabulationError::ZeroSize)
  // }
}

#[derive(Debug)]
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

  fn count_below_or_equal(&self, value: u8) -> u8 {
    self.below_or_equal(value).count() as u8
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
    
    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationStepOutcome::Win{ place: 1u8, who } => *who == competitor_0,
      _ => false
    });
    assert!(match outcomes[1] {
      TabulationStepOutcome::Win{ place: 2u8, who } => *who == competitor_1,
      _ => false
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison { favored, unfavored, ref factor } = products[0];
    assert!(*favored == competitor_0);
    assert!(*unfavored == competitor_1);
    assert!(match factor {
      ResultFactor::ClearMajority => true,
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
    
    let outcomes = &step_results.outcomes;
    assert!(match outcomes[0] {
      TabulationStepOutcome::Tie{ place: 1u8, who: ref tied } => {
        tied.len() == 2 &&
        tied.iter().any(|&scores| *scores.competitor == competitor_0) &&
        tied.iter().any(|&scores| *scores.competitor == competitor_1)
      },
      _ => false
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 0);
  }

  #[test]
  fn greater_majority_step_win() {
    let judges = generate_random_judges(3);
    let competitor_0 = Competitor::random();
    let ordinals_0 = CompetitorOrdinals {
      competitor: &competitor_0,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 2,
        &judges[2] => 3
      )
    };
    let competitor_1 = Competitor::random();
    let ordinals_1 = CompetitorOrdinals {
      competitor: &competitor_1,
      ordinal_scores: map!(
        &judges[0] => 2,
        &judges[1] => 1,
        &judges[2] => 2
      )
    };
    let step_results = Tabulation::greater_majority_step(1u8, vec![&ordinals_0, &ordinals_1]);
    
    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationStepOutcome::Win{ place: 1u8, who } => *who == competitor_1,
      _ => false
    });
    assert!(match outcomes[1] {
      TabulationStepOutcome::Win{ place: 2u8, who } => *who == competitor_0,
      _ => false
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison { favored, unfavored, ref factor } = products[0];
    assert!(*favored == competitor_1);
    assert!(*unfavored == competitor_0);
    assert!(match factor {
      ResultFactor::GreaterMajority => true,
      _ => false
    });
  }

  #[test]
  fn sum_below_step_win() {
    let judges = generate_random_judges(5);
    let competitor_0 = Competitor::random();
    let ordinals_0 = CompetitorOrdinals {
      competitor: &competitor_0,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 2,
        &judges[2] => 3,
        &judges[3] => 4,
        &judges[4] => 5
      )
    };
    let competitor_1 = Competitor::random();
    let ordinals_1 = CompetitorOrdinals {
      competitor: &competitor_1,
      ordinal_scores: map!(
        &judges[0] => 5,
        &judges[1] => 4,
        &judges[2] => 3,
        &judges[3] => 1,
        &judges[4] => 1
      )
    };
    let step_results = Tabulation::sum_below_step(1u8, vec![&ordinals_0, &ordinals_1]);
    
    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationStepOutcome::Win{ place: 1u8, who } => *who == competitor_1,
      _ => false
    });
    assert!(match outcomes[1] {
      TabulationStepOutcome::Win{ place: 2u8, who } => *who == competitor_0,
      _ => false
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison { favored, unfavored, ref factor } = products[0];
    assert!(*favored == competitor_1);
    assert!(*unfavored == competitor_0);
    assert!(match factor {
      ResultFactor::SumBelow => true,
      _ => false
    });
  }

  fn following_score_step_win() {
    let judges = generate_random_judges(5);
    let competitor_0 = Competitor::random();
    let ordinals_0 = CompetitorOrdinals {
      competitor: &competitor_0,
      ordinal_scores: map!(
        &judges[0] => 1,
        &judges[1] => 2,
        &judges[2] => 3,
        &judges[3] => 4,
        &judges[4] => 4
      )
    };
    let competitor_1 = Competitor::random();
    let ordinals_1 = CompetitorOrdinals {
      competitor: &competitor_1,
      ordinal_scores: map!(
        &judges[0] => 5,
        &judges[1] => 4,
        &judges[2] => 3,
        &judges[3] => 2,
        &judges[4] => 1
      )
    };
    let step_results = Tabulation::following_score_step(1u8, vec![&ordinals_0, &ordinals_1]);
    
    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationStepOutcome::Win{ place: 1u8, who } => *who == competitor_0,
      _ => false
    });
    assert!(match outcomes[1] {
      TabulationStepOutcome::Win{ place: 2u8, who } => *who == competitor_1,
      _ => false
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison { favored, unfavored, ref factor } = products[0];
    assert!(*favored == competitor_1);
    assert!(*unfavored == competitor_0);
    assert!(match factor {
      ResultFactor::FollowingScore => true,
      _ => false
    });
  }
}