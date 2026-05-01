![mit](https://img.shields.io/badge/License-MIT-blue.svg)

# "I would not have done that": Outcome knowledge distorts memory for decisions made minutes ago

**Keishiro Sawa, Carla Zoe Cremer, Tobias Gerstenberg, Sanjay Manohar & Matan Mazor**

*University of Oxford & Stanford University*

> Remembering one's past decisions is critical for learning from delayed feedback and maintaining an accurate self-model. Across two gamified experimental paradigms and three experiments (total N = 500), we find that a selective forgetting of unsuccessful guesses operates within minutes, that it evades introspection, and that it affects confidence ratings in memory judgments. Analysis of memory errors reveals that, in reconstructing their past decisions, people integrate what they remember deciding, what they believe they would have decided, and what they would decide now if asked to make the same decision again.

Sawa, K., Cremer, C. Z., Gerstenberg, T., Manohar, S., & Mazor, M. (2026). "I would not have done that": Outcome knowledge distorts memory for decisions made minutes ago. *Proceedings of the 48th Annual Conference of the Cognitive Science Society*. [[PDF]](docs/cogsci26Sawa.pdf)

---

> **Note on experiment numbering:** Experiments in this repository are numbered differently from the paper. Paper Exp. 1 → `experiments/symmetry`, Paper Exp. 2 → `experiments/experiment1`, Paper Exp. 3 → `experiments/experiment2`. `experiment3` and `experiment4` are pilots not reported in the CogSci paper.

All three experiments were pre-registered on OSF. In addition, to ensure preregistration time-locking (in other words, that preregistration preceded data collection), we employed [randomization-based preregistration](https://medium.com/@mazormatan/cryptographic-preregistration-from-newton-to-fmri-df0968377bb2). EWe used the SHA256 cryptographic hash function to translate our preregistered protocol folder (including the pre-registration document) to a string of 256 bits. These bits were then combined with the unique identifiers of single subjects, and the resulting string was used as seed for initializing the Mersenne Twister pseudorandom number generator prior to determining all random aspects of the experiment, including box labels, numbers, and the location of gems. This way, experimental randomization was causally dependent on, and therefore could not have been determined prior to, the specific contents of our preregistration document ([Mazor, Mazor & Mukamel, 2019](https://doi.org/10.1111/ejn.14278)).Experiment 1 was not cryptographically pre-registered because the stimulus sequence had insufficient entropy to support randomization-based commitment. 
---

## Experiment 1 — Grid game: selective forgetting of wrong guesses

Participants revealed hidden black squares in a 6×6 grid, then reproduced their exact guess sequence from memory minutes later. A between-subjects manipulation exposed half of participants to symmetric game boards, allowing them to learn a hidden regularity before the memory phase.

- **Try it:** [symmetric version](https://self-model.github.io/memory_reconstruction_cogsci26/experiments/demos/symmetry/symmetric/experiment.html) | [asymmetric version](https://self-model.github.io/memory_reconstruction_cogsci26/experiments/demos/symmetry/asymmetric/experiment.html)
- **Pre-registration:** [OSF.IO/3MPB4](https://osf.io/3mpb4) — `docs/pre-registrations/prereg_symmetry/`
- **Data:** `data/symmetry/`
- **Experiment code:** `experiments/symmetry/`
- **Analysis:** `analysis/reportSymmetry.Rmd`

## Experiment 2 — Gem-box game: learning distorts memory for own decisions

On each trial, participants chose between three labelled boxes to collect hidden gems, then reproduced their exact choice sequence from memory. Gem locations followed a hidden positional rule, allowing us to test whether learning the rule distorted memory of earlier decisions made before the rule was known.

- **Try it:** [here](https://self-model.github.io/memory_reconstruction_cogsci26/experiments/demos/experiment1/experiment.html)
- **Pre-registration:** [OSF.IO/K8AT9](https://osf.io/k8at9) — `docs/pre-registrations/prereg1/`
- **Protocol-folder**: [here](https://self-model.github.io/memory_reconstruction_cogsci26/experiments/experiment1/exp1_protocolFolder.zip)
- **Protocol sum** (for [randomization-based preregistration](https://medium.com/@mazormatan/cryptographic-preregistration-from-newton-to-fmri-df0968377bb2)):  bb0b97cd13d98339719f449913a4057bc8e90e66bccd00418d88bb9801e26165
- **Data:** `data/experiment1/`
- **Experiment code:** `experiments/experiment1/`
- **Analysis:** `analysis/exp1_standardised.Rmd`

## Experiment 3 — Gem-box with explicit rule: distortions are not strengthened by explicit knowledge

A replication of Experiment 2 in which participants were explicitly told the hidden rule and shown the true gem location on each memory trial, and rated their confidence after each response. This allowed us to test whether outcome-based memory distortions reflect conscious or unconscious processes.

- **Try it:** [here](https://self-model.github.io/memory_reconstruction_cogsci26/experiments/demos/experiment2/experiment.html)
- **Pre-registration:** [OSF.IO/RDBS7](https://osf.io/rdbs7) — `docs/pre-registrations/prereg2/`
- **Protocol-folder**: [here](https://self-model.github.io/memory_reconstruction_cogsci26/experiments/experiment1/exp2_protocolFolder.zip)
- **Protocol sum**:  7e03badefef6f8c396c225f9b777f4dc738377f336a6e74b80ca1e9b99db9e84
- **Data:** `data/experiment2/`
- **Experiment code:** `experiments/experiment2/`
- **Analysis:** `analysis/exp2_standardised.Rmd`

