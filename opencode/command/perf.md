---
description: Mesure avant d'optimiser (hyperfine / profilage)
---

Optimise : $ARGUMENTS

Règle absolue : **mesure d'abord**. Une optimisation sans chiffre avant/après
n'est pas une optimisation, c'est une supposition.

1. Établis la mesure de référence. Pour une commande : `hyperfine --warmup 3`.
   Pour du code : le profileur du langage (`cProfile`, `pytest-benchmark`,
   `cargo bench`, `go test -bench`).
2. Identifie où va réellement le temps. Ne devine pas — le goulot est
   rarement là où on le croit.
3. Change **une** chose.
4. Re-mesure et donne le delta chiffré.

Si le gain est inférieur à ~10 %, dis-le et propose de ne rien changer :
la complexité ajoutée coûte plus que le gain.
