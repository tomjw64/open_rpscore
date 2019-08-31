# Open RP Score
A library in Rust that provides an implementation of relative placement scoring. While not quite useable yet, it is based on and tested against all examples on rp-scoring.com, providing a tested implementation of relative placement finals scoring from start to finish, including all steps of the scoring process. Open RP Score does not allow ending the competition with ties currently, since it runs the tabulation to its completion, including a final tie-break from a head judge.

## Plans
I hope to eventually deploy this somewhere along with a scoresheet generator, so that people and dance competition organizers can make use of relative placement scoring freely, quickly, and without having to know Rust or how relative placement works.

## Important functions
`Tabulation::full` tabulates a relative placement finals competition from a slice of `CompetitorOrdinals`, yielding either a `Tabulation` or a `TabulationError`
