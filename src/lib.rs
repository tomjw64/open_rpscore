#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Debug)]
struct Competitor {
  name: String,
}

impl Competitor {
  fn from(name: &str) -> Competitor {
    Competitor {
      name: String::from(name),
    }
  }
}

#[derive(Hash, Eq, PartialEq, Debug)]
struct Judge {
  name: String,
  head: bool,
}

#[derive(Debug)]
struct CompetitorOrdinals<'a> {
  competitor: &'a Competitor,
  ordinal_scores: HashMap<&'a Judge, u8>,
}

#[derive(PartialEq, Debug)]
enum ResultFactor {
  ClearMajority,
  GreaterMajority,
  SumBelow,
  FollowingScore,
  InstantRunoff,
  HeadJudge,
}

#[derive(PartialEq, Debug)]
enum TabulationError {
  ZeroSize,
  NoHeadJudge,
  DataIncomplete,
  OrdinalsDiscontinuous,
}

#[derive(Debug)]
struct ResultComparison<'a> {
  favored: &'a Competitor,
  unfavored: &'a Competitor,
  factor: ResultFactor,
}

#[derive(Debug)]
enum TabulationOutcome<'a> {
  Win {
    place: u8,
    who: &'a Competitor,
  },
  Tie {
    place: u8,
    who: Vec<&'a CompetitorOrdinals<'a>>,
  },
}

#[derive(Debug)]
struct Tabulation<'a> {
  outcomes: Vec<TabulationOutcome<'a>>,
  products: Vec<ResultComparison<'a>>,
}

impl CompetitorOrdinals<'_> {
  fn minimize<'a, 'b>(initial: &[&'a CompetitorOrdinals<'b>]) -> Vec<CompetitorOrdinals<'b>> {
    let num_competitors = initial.len();
    let judges = initial[0].ordinal_scores.keys();
    let mut grouped_hash: HashMap<&'a Judge, Vec<u8>> = HashMap::new();
    judges.for_each(|&judge| {
      grouped_hash.insert(
        judge,
        initial
          .iter()
          .map(move |scores| {
            *scores
              .ordinal_scores
              .get(judge)
              .expect("Ordinals have not been validated.")
          })
          .sorted()
          .collect(),
      );
    });

    initial
      .iter()
      .map(|scores| CompetitorOrdinals {
        competitor: scores.competitor,
        ordinal_scores: scores
          .ordinal_scores
          .iter()
          .map(|(&key, value)| {
            (
              key,
              grouped_hash
                .get(key)
                .expect("Ordinals have not been validated")
                .iter()
                .position(|maybe_value| maybe_value == value)
                .expect("Ordinals have not been validated") as u8,
            )
          })
          .collect(),
      })
      .collect()
  }

  fn head_judge_score(&self) -> u8 {
    *self
      .ordinal_scores
      .iter()
      .find(|(judge, ordinal)| judge.head)
      .expect("Ordinals have not been validated")
      .1
  }

  fn below_or_equal(&self, value: u8) -> impl Iterator<Item = &u8> {
    self
      .ordinal_scores
      .values()
      .filter(move |&&ordinal| ordinal <= value)
  }

  fn ordinals_cloned(&self) -> Vec<u8> {
    self.ordinal_scores.values().cloned().collect()
  }

  fn sorted_ordinals(&self) -> Vec<u8> {
    let mut ordinals_clone = self.ordinals_cloned();
    ordinals_clone.sort();
    ordinals_clone
  }

  fn majority_reached_ordinal(&self) -> u8 {
    let majority_index = (self.ordinal_scores.len() as f64 / 2f64).floor() as usize;
    self.sorted_ordinals()[majority_index]
  }

  fn sum_below_or_equal(&self, value: u8) -> u8 {
    self.below_or_equal(value).sum()
  }

  fn count_below_or_equal(&self, value: u8) -> u8 {
    self.below_or_equal(value).count() as u8
  }

  fn counts_below_all_ordinals(&self) -> Vec<u8> {
    (1..=self.ordinal_scores.len())
      .map(|ordinal| self.count_below_or_equal(ordinal as u8))
      .collect()
  }
}

impl Tabulation<'_> {
  fn comparison_step<T: PartialEq, G: Fn(&CompetitorOrdinals) -> T, S: Fn(&T, &T) -> Ordering>(
    group_function: G,
    sort_function: S,
    factor: impl Fn() -> ResultFactor,
  ) -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals<'a>]) -> Tabulation<'a> {
    move |place, tied| {
      let mut result = Tabulation {
        outcomes: vec![],
        products: vec![],
      };
      let sorted_groups: Vec<Vec<&CompetitorOrdinals>> = tied
        .iter()
        .group_by(|scores| group_function(scores))
        .into_iter()
        .sorted_by(|(key, _), (key_other, _)| sort_function(key, key_other))
        .map(|(_, group)| group.cloned().collect())
        .collect();

      let mut offset = 0u8;
      let mut traversed: Vec<&CompetitorOrdinals> = vec![];

      sorted_groups.iter().for_each(|group| {
        result.outcomes.push(match group.len() {
          0 => panic!("Unexpected zero size group"),
          1 => TabulationOutcome::Win {
            place: place + offset,
            who: group[0].competitor,
          },
          _ => TabulationOutcome::Tie {
            place: place + offset,
            who: group.clone(),
          },
        });

        traversed.iter().for_each(|t_competitor_scores| {
          group.iter().for_each(|competitor_scores| {
            result.products.push(ResultComparison {
              favored: t_competitor_scores.competitor,
              unfavored: competitor_scores.competitor,
              factor: factor(),
            });
          });
        });

        traversed.extend(group);
        offset += group.len() as u8;
      });

      result
    }
  }

  fn clear_majority_step() -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals<'a>]) -> Tabulation<'a> {
    let group_function = |scores: &CompetitorOrdinals| scores.majority_reached_ordinal();
    let sort_function = |key: &u8, key_other: &u8| key.cmp(key_other);
    let factor = || ResultFactor::ClearMajority;
    Tabulation::comparison_step(group_function, sort_function, factor)
  }

  fn greater_majority_step() -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals<'a>]) -> Tabulation<'a>
  {
    let group_function =
      |scores: &CompetitorOrdinals| scores.count_below_or_equal(scores.majority_reached_ordinal());
    let sort_function = |key: &u8, key_other: &u8| key.cmp(key_other).reverse();
    let factor = || ResultFactor::GreaterMajority;
    Tabulation::comparison_step(group_function, sort_function, factor)
  }

  fn sum_below_step() -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals<'a>]) -> Tabulation<'a> {
    let group_function =
      |scores: &CompetitorOrdinals| scores.sum_below_or_equal(scores.majority_reached_ordinal());
    let sort_function = |key: &u8, key_other: &u8| key.cmp(key_other);
    let factor = || ResultFactor::SumBelow;
    Tabulation::comparison_step(group_function, sort_function, factor)
  }

  fn following_score_step() -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals<'a>]) -> Tabulation<'a>
  {
    fn u8_slice_compare(slice: &[u8], other_slice: &[u8]) -> Ordering {
      let zipped: Vec<(u8, &u8)> = slice.iter().cloned().zip(other_slice).collect();
      for (slice_u8, other_slice_u8) in zipped {
        match slice_u8.cmp(other_slice_u8) {
          Ordering::Equal => {}
          other => return other,
        }
      }
      Ordering::Equal
    }

    let group_function = |scores: &CompetitorOrdinals| scores.counts_below_all_ordinals();
    let sort_function =
      |key: &Vec<u8>, key_other: &Vec<u8>| u8_slice_compare(key, key_other).reverse();
    let factor = || ResultFactor::FollowingScore;
    Tabulation::comparison_step(group_function, sort_function, factor)
  }

  fn runoff_step() -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals]) -> Tabulation<'a> {
    |place, tied| {
      let minimized = CompetitorOrdinals::minimize(tied);
      let minimized_refs: Vec<&CompetitorOrdinals> = minimized.iter().collect();

      let comparison_steps: [Box<
        dyn for<'b> Fn(u8, &[&'b CompetitorOrdinals<'b>]) -> Tabulation<'b>,
      >; 4] = [
        Box::new(Tabulation::clear_majority_step()),
        Box::new(Tabulation::greater_majority_step()),
        Box::new(Tabulation::sum_below_step()),
        Box::new(Tabulation::following_score_step()),
      ];

      let initial = Tabulation {
        outcomes: vec![TabulationOutcome::Tie {
          place: 1u8,
          who: minimized_refs,
        }],
        products: vec![],
      };
      let minimized_comparison_round = comparison_steps.iter().fold(initial, |collapsed, step| {
        Tabulation::step_collapse(collapsed, step)
      });

      let mut outcomes_from_tied = vec![];
      minimized_comparison_round
        .outcomes
        .iter()
        .for_each(|outcome| match outcome {
          TabulationOutcome::Tie {
            place: minimized_place,
            who: minimized_who,
          } => {
            let mut tied_competitors = vec![];
            minimized_who.iter().for_each(|mini_score| {
              tied_competitors.push(
                *tied
                  .iter()
                  .find(|score| score.competitor == mini_score.competitor)
                  .expect("Same competitors should be in each group"),
              )
            });
            outcomes_from_tied.push(TabulationOutcome::Tie {
              place: place + (minimized_place - 1),
              who: tied_competitors,
            });
          }
          TabulationOutcome::Win {
            place: minimized_place,
            who: minimized_who,
          } => outcomes_from_tied.push(TabulationOutcome::Win {
            place: place + (minimized_place - 1),
            who: tied
              .iter()
              .find(|score| &score.competitor == minimized_who)
              .expect("Same competitors should be in each group")
              .competitor,
          }),
        });

      let mut products_from_tied = vec![];
      minimized_comparison_round
        .products
        .iter()
        .for_each(|comparison| {
          let ResultComparison {
            favored,
            unfavored,
            factor,
          } = comparison;
          products_from_tied.push(ResultComparison {
            favored: tied
              .iter()
              .find(|score| &score.competitor == favored)
              .expect("Same competitors should be in each group")
              .competitor,
            unfavored: tied
              .iter()
              .find(|score| &score.competitor == unfavored)
              .expect("Same competitors should be in each group")
              .competitor,
            factor: ResultFactor::InstantRunoff,
          })
        });

      Tabulation {
        outcomes: outcomes_from_tied,
        products: products_from_tied,
      }
    }
  }

  fn head_judge_tie_break() -> impl for<'a> Fn(u8, &[&'a CompetitorOrdinals<'a>]) -> Tabulation<'a>
  {
    let group_function = |scores: &CompetitorOrdinals| scores.head_judge_score();
    let sort_function = |key: &u8, key_other: &u8| key.cmp(key_other);
    let factor = || ResultFactor::HeadJudge;
    Tabulation::comparison_step(group_function, sort_function, factor)
  }

  fn step_collapse<'a>(
    step_results: Tabulation<'a>,
    next_step: &dyn for<'b> Fn(u8, &[&'b CompetitorOrdinals<'b>]) -> Tabulation<'b>,
  ) -> Tabulation<'a> {
    let mut collapse_outcomes: Vec<TabulationOutcome<'a>> = vec![];
    let mut collapse_products: Vec<ResultComparison<'a>> = step_results.products;
    step_results.outcomes.into_iter().for_each(|outcome| {
      match outcome {
        TabulationOutcome::Win { place, who } => collapse_outcomes.push(outcome),
        TabulationOutcome::Tie { place, who } => {
          let next_step_results = next_step(place, &who);
          collapse_outcomes.extend(next_step_results.outcomes.into_iter());
          collapse_products.extend(next_step_results.products.into_iter());
        }
      };
    });
    Tabulation {
      outcomes: collapse_outcomes,
      products: collapse_products,
    }
  }

  fn validate(scores: &[&CompetitorOrdinals]) -> Option<TabulationError> {
    let num_competitors = scores.len() as u8;
    if num_competitors == 0 {
      return Some(TabulationError::ZeroSize);
    }

    let num_judges = scores[0].ordinal_scores.len();
    if scores
      .iter()
      .any(|score| score.ordinal_scores.len() != num_judges)
    {
      return Some(TabulationError::DataIncomplete);
    }

    if scores[0].ordinal_scores.keys().all(|judge| !judge.head) {
      return Some(TabulationError::NoHeadJudge);
    }

    let mut discontinuity = false;
    let mut judges_scores_remaining: HashMap<&Judge, Vec<Option<u8>>> = HashMap::new();
    scores[0].ordinal_scores.keys().for_each(|&judge| {
      judges_scores_remaining.insert(judge, (1..=num_competitors).map(|i| Some(i)).collect());
    });
    scores.iter().for_each(|score| {
      if discontinuity {
        return;
      }
      score.ordinal_scores.iter().for_each(|(judge, ordinal)| {
        if ordinal > &num_competitors {
          discontinuity = true;
          println!("Ordinal too big: {:?}!", ordinal);
          return;
        }
        let mut original = judges_scores_remaining
          .get_mut(judge)
          .expect("Judges are from the same set")
          .get_mut((ordinal - 1) as usize)
          .expect("Vec size unchanged");
        match original {
          Some(_) => {
            *original = None;
          }
          None => {
            discontinuity = true;
            println!("Dupe: {:?}", ordinal - 1);
            return;
          }
        };
      });
    });
    if judges_scores_remaining
      .values()
      .any(|scores_each| scores_each.iter().any(|score| score.is_some()))
    {
      discontinuity = true
    }
    if discontinuity {
      return Some(TabulationError::OrdinalsDiscontinuous);
    }

    None
  }

  fn full<'a>(scores: &[&'a CompetitorOrdinals<'a>]) -> Result<Tabulation<'a>, TabulationError> {
    match Tabulation::validate(scores) {
      Some(error) => return Err(error),
      None => {}
    }

    let comparison_steps: [Box<dyn for<'b> Fn(u8, &[&'b CompetitorOrdinals<'b>]) -> Tabulation<'b>>;
      6] = [
      Box::new(Tabulation::clear_majority_step()),
      Box::new(Tabulation::greater_majority_step()),
      Box::new(Tabulation::sum_below_step()),
      Box::new(Tabulation::following_score_step()),
      Box::new(Tabulation::runoff_step()),
      Box::new(Tabulation::head_judge_tie_break()),
    ];

    let initial = Tabulation {
      outcomes: vec![TabulationOutcome::Tie {
        place: 1u8,
        who: scores.to_vec(),
      }],
      products: vec![],
    };
    let tabulation = comparison_steps.iter().fold(initial, |collapsed, step| {
      Tabulation::step_collapse(collapsed, step)
    });

    Ok(tabulation)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use rand::distributions::Alphanumeric;
  use rand::{thread_rng, Rng};
  use std::iter;

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

  fn generate_random_judges_no_head(number: usize) -> Vec<Judge> {
    iter::repeat_with(|| Judge::random(false))
      .take(number)
      .collect()
  }

  fn generate_random_competitors(number: usize) -> Vec<Competitor> {
    iter::repeat_with(|| Competitor::random())
      .take(number)
      .collect()
  }

  fn generate_ordinals<'a, 'b>(
    judges: &'a [Judge],
    competitors: &'a [Competitor],
    table: Vec<Vec<u8>>,
  ) -> Vec<CompetitorOrdinals<'a>> {
    competitors
      .iter()
      .zip(table)
      .map(|(competitor, ordinals)| {
        let ordinal_scores: HashMap<_, _> = judges.iter().zip(ordinals).collect();
        CompetitorOrdinals {
          competitor,
          ordinal_scores,
        }
      })
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
        name: generate_random_name(8),
      }
    }
  }

  impl Judge {
    fn random(head: bool) -> Judge {
      Judge {
        name: generate_random_name(8),
        head,
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
      ),
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
      ),
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
      ),
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
      ),
    };

    assert_eq!(ordinals.majority_reached_ordinal(), 2);
  }

  #[test]
  fn clear_majority_step_win() {
    let judges = generate_random_judges(4);
    let competitors = generate_random_competitors(2);
    let scores_table = vec![vec![1, 1, 1, 2], vec![2, 2, 2, 1]];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let step_results = Tabulation::clear_majority_step()(1u8, &scores_refs);

    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[0],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[1],
      _ => false,
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison {
      favored,
      unfavored,
      ref factor,
    } = products[0];
    assert!(*favored == competitors[0]);
    assert!(*unfavored == competitors[1]);
    assert!(match factor {
      ResultFactor::ClearMajority => true,
      _ => false,
    });
  }

  #[test]
  fn clear_majority_step_tie() {
    let judges = generate_random_judges(4);
    let competitors = generate_random_competitors(2);
    let scores_table = vec![vec![1, 1, 2, 2], vec![2, 2, 1, 1]];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let step_results = Tabulation::clear_majority_step()(1u8, &scores_refs);

    let outcomes = &step_results.outcomes;
    assert!(match outcomes[0] {
      TabulationOutcome::Tie {
        place: 1u8,
        who: ref tied,
      } => {
        tied.len() == 2
          && tied
            .iter()
            .any(|&scores| *scores.competitor == competitors[0])
          && tied
            .iter()
            .any(|&scores| *scores.competitor == competitors[1])
      }
      _ => false,
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 0);
  }

  #[test]
  fn greater_majority_step_win() {
    let judges = generate_random_judges(3);
    let competitors = generate_random_competitors(2);
    let scores_table = vec![vec![1, 2, 3], vec![2, 1, 2]];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let step_results = Tabulation::greater_majority_step()(1u8, &scores_refs);

    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[1],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[0],
      _ => false,
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison {
      favored,
      unfavored,
      ref factor,
    } = products[0];
    assert!(*favored == competitors[1]);
    assert!(*unfavored == competitors[0]);
    assert!(match factor {
      ResultFactor::GreaterMajority => true,
      _ => false,
    });
  }

  #[test]
  fn sum_below_step_win() {
    let judges = generate_random_judges(5);
    let competitors = generate_random_competitors(2);
    let scores_table = vec![vec![1, 2, 3, 4, 5], vec![5, 4, 3, 1, 1]];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let step_results = Tabulation::sum_below_step()(1u8, &scores_refs);

    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[1],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[0],
      _ => false,
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison {
      favored,
      unfavored,
      ref factor,
    } = products[0];
    assert!(*favored == competitors[1]);
    assert!(*unfavored == competitors[0]);
    assert!(match factor {
      ResultFactor::SumBelow => true,
      _ => false,
    });
  }

  #[test]
  fn following_score_step_win() {
    let judges = generate_random_judges(5);
    let competitors = generate_random_competitors(2);
    let scores_table = vec![vec![1, 2, 3, 4, 4], vec![5, 4, 3, 2, 1]];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let step_results = Tabulation::following_score_step()(1u8, &scores_refs);

    let outcomes = &step_results.outcomes;
    assert_eq!(outcomes.len(), 2);
    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[0],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[1],
      _ => false,
    });

    let products = &step_results.products;
    assert_eq!(products.len(), 1);
    let ResultComparison {
      favored,
      unfavored,
      ref factor,
    } = products[0];
    assert!(*favored == competitors[0]);
    assert!(*unfavored == competitors[1]);
    assert!(match factor {
      ResultFactor::FollowingScore => true,
      _ => false,
    });
  }

  #[test]
  fn com_rp_scoring_determine_majority() {
    let judges = generate_random_judges(5);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 3, 1, 1, 3],
      vec![2, 1, 2, 3, 2],
      vec![3, 2, 4, 2, 1],
      vec![4, 4, 3, 4, 4],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap();

    let outcomes = &results.outcomes;

    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[0],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[1],
      _ => false,
    });
    assert!(match outcomes[2] {
      TabulationOutcome::Win { place: 3u8, who } => *who == competitors[2],
      _ => false,
    });
    assert!(match outcomes[3] {
      TabulationOutcome::Win { place: 4u8, who } => *who == competitors[3],
      _ => false,
    });

    let products = &results.products;

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[0]
          && *product.unfavored == competitors[1]
          && product.factor == ResultFactor::ClearMajority
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[1]
          && *product.unfavored == competitors[2]
          && product.factor == ResultFactor::GreaterMajority
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[2]
          && *product.unfavored == competitors[3]
          && product.factor == ResultFactor::ClearMajority
      })
      .is_some());
  }

  #[test]
  fn com_rp_scoring_equal_majority() {
    let judges = generate_random_judges(5);
    let competitors = generate_random_competitors(5);
    let scores_table = vec![
      vec![3, 2, 1, 1, 4],
      vec![2, 1, 3, 3, 2],
      vec![1, 4, 4, 2, 3],
      vec![5, 3, 2, 4, 1],
      vec![4, 5, 5, 5, 5],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap();

    let outcomes = &results.outcomes;

    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[0],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[1],
      _ => false,
    });
    assert!(match outcomes[2] {
      TabulationOutcome::Win { place: 3u8, who } => *who == competitors[2],
      _ => false,
    });
    assert!(match outcomes[3] {
      TabulationOutcome::Win { place: 4u8, who } => *who == competitors[3],
      _ => false,
    });
    assert!(match outcomes[4] {
      TabulationOutcome::Win { place: 5u8, who } => *who == competitors[4],
      _ => false,
    });

    let products = &results.products;

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[0]
          && *product.unfavored == competitors[1]
          && product.factor == ResultFactor::SumBelow
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[1]
          && *product.unfavored == competitors[2]
          && product.factor == ResultFactor::ClearMajority
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[2]
          && *product.unfavored == competitors[3]
          && product.factor == ResultFactor::FollowingScore
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[3]
          && *product.unfavored == competitors[4]
          && product.factor == ResultFactor::ClearMajority
      })
      .is_some());
  }

  #[test]
  fn com_rp_scoring_two_equal_scores() {
    let judges = generate_random_judges(5);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 2, 1, 1, 3],
      vec![2, 1, 3, 3, 4],
      vec![3, 3, 2, 4, 1],
      vec![4, 4, 4, 2, 2],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap();

    let outcomes = &results.outcomes;

    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[0],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[1],
      _ => false,
    });
    assert!(match outcomes[2] {
      TabulationOutcome::Win { place: 3u8, who } => *who == competitors[2],
      _ => false,
    });
    assert!(match outcomes[3] {
      TabulationOutcome::Win { place: 4u8, who } => *who == competitors[3],
      _ => false,
    });

    let products = &results.products;

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[0]
          && *product.unfavored == competitors[1]
          && product.factor == ResultFactor::ClearMajority
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[1]
          && *product.unfavored == competitors[2]
          && product.factor == ResultFactor::InstantRunoff
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[2]
          && *product.unfavored == competitors[3]
          && product.factor == ResultFactor::ClearMajority
      })
      .is_some());
  }

  #[test]
  fn com_rp_scoring_completely_equal() {
    let judges = generate_random_judges(4);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 2, 3, 4],
      vec![2, 3, 4, 1],
      vec![3, 4, 1, 2],
      vec![4, 1, 2, 3],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap();

    let outcomes = &results.outcomes;

    assert!(match outcomes[0] {
      TabulationOutcome::Win { place: 1u8, who } => *who == competitors[0],
      _ => false,
    });
    assert!(match outcomes[1] {
      TabulationOutcome::Win { place: 2u8, who } => *who == competitors[1],
      _ => false,
    });
    assert!(match outcomes[2] {
      TabulationOutcome::Win { place: 3u8, who } => *who == competitors[2],
      _ => false,
    });
    assert!(match outcomes[3] {
      TabulationOutcome::Win { place: 4u8, who } => *who == competitors[3],
      _ => false,
    });

    let products = &results.products;

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[0]
          && *product.unfavored == competitors[1]
          && product.factor == ResultFactor::HeadJudge
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[1]
          && *product.unfavored == competitors[2]
          && product.factor == ResultFactor::HeadJudge
      })
      .is_some());

    assert!(products
      .iter()
      .find(|product| {
        *product.favored == competitors[2]
          && *product.unfavored == competitors[3]
          && product.factor == ResultFactor::HeadJudge
      })
      .is_some());
  }

  #[test]
  fn error_no_head_judge() {
    let judges = generate_random_judges_no_head(4);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 1, 1, 1],
      vec![2, 2, 2, 2],
      vec![3, 3, 3, 3],
      vec![4, 4, 4, 4],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap_err();

    assert!(match results {
      TabulationError::NoHeadJudge => true,
      _ => false,
    });
  }

  fn error_no_scores() {
    let scores_refs: Vec<&CompetitorOrdinals> = vec![];

    let results = Tabulation::full(&scores_refs).unwrap_err();

    assert!(match results {
      TabulationError::ZeroSize => true,
      _ => false,
    });
  }

  #[test]
  fn error_data_incomplete_missing() {
    let judges = generate_random_judges(4);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 1, 1],
      vec![2, 2, 2, 2],
      vec![3, 3, 3, 3],
      vec![4, 4, 4, 4],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap_err();

    assert!(match results {
      TabulationError::DataIncomplete => true,
      _ => false,
    });
  }

  #[test]
  fn error_ordinals_discontinuous_dupe() {
    let judges = generate_random_judges(4);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 1, 1, 1],
      vec![2, 2, 2, 2],
      vec![3, 3, 3, 3],
      vec![4, 4, 2, 4],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap_err();

    assert!(match results {
      TabulationError::OrdinalsDiscontinuous => true,
      _ => false,
    });
  }

  #[test]
  fn error_ordinals_discontinuous_too_high() {
    let judges = generate_random_judges(4);
    let competitors = generate_random_competitors(4);
    let scores_table = vec![
      vec![1, 1, 1, 1],
      vec![2, 2, 2, 2],
      vec![3, 3, 3, 3],
      vec![4, 4, 5, 4],
    ];
    let scores = generate_ordinals(&judges, &competitors, scores_table);
    let scores_refs: Vec<&CompetitorOrdinals> = scores.iter().collect();

    let results = Tabulation::full(&scores_refs).unwrap_err();

    assert!(match results {
      TabulationError::OrdinalsDiscontinuous => true,
      _ => false,
    });
  }
}
