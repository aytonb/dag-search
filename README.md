# DAG Search

## Purpose

This package implements search for optimal directed acyclic graphs (DAGs) using A* with bounding conflicts, as described in the paper "DAG-GPs: Learning Directed Acyclic Graph Structure For Multi-Output Gaussian Processes". Search is designed to reach optimality while evaluating as few likelihoods as possible, making it valuable when likelihoods are computationally expensive to evaluate.

For benchmarking purposes, this package also includes implementations of A* and tabu search.

This package is implemented in Common Lisp. It has only been tested in SBCL.


## Dependencies

The following dependencies **available in quicklisp** are required:

- [priority-queue](https://github.com/dsorokin/priority-queue) (Custom permissive license)


## Getting Started

This package can be loaded with quicklisp or asdf.

Simple setup and usage for a problem with 3 variables and 50 data points looks like this:

```
(use-package :dag-search)

;; Make a score object
(defparameter score (make-instance 'gp-bic-score
                                   :n-vars 3
                                   :n-data-points 50))

;; Make an initial search node
(defparameter initial-node (make-initial-search-node 3))

;; Determine k-groups for heuristics
(defparameter k-groups '((0) (1) (2)))

;; Define a function that returns log likelihood
(defun log-lik-fn (variable parents)
  ;; Expensive to compute function here...
)

;; Run A*BC search, returns (terminal-node queue scores expanded-list)
(defparameter abc-out (abc-search initial-node score #'log-lik-fn k-groups))

;; Extract the best DAG edges
(edges (first abc-out))
```

`abc-search` returns a list `(terminal-node queue scores expanded-list)`. `terminal-node` is the final expanded search node, `queue` is the remaining search queue, `scores` is a structure containing all evaluated scores, and `expanded-list` is an expanded list used in A* search. All information about the best DAG is in `terminal-node`. The remaining outputs are primarily used for debug purposes or to extract statistics about the solution process, like how many times the likelihood function was run.


## Advanced Options

### Score Objects

Score objects dictate how the addition of edges in the DAG should be penalized. dag-search exports a number of different score objects, but so far they are rather specialized. Currently exported are:

- `gp-aic-score`: AIC score for DAG-GPs
- `gp-bic-score`: BIC score for DAG-GPs
- `discrete-aic-score`: AIC score for discrete variable problems
- `discrete-bic-score`: BIC score for discrete variable problems
- `custom-score`: A custom defined score

All score objects maximize optimal log likelihood subtract a penalization term. AIC scores subtract a penalization term equal to the number of parameters per parent. BIC scores subtract a penalization term equal to $\log N / 2$ times the number of parameters per parent for $N$ data points. GP scores state there is one parameter per parent, discrete scores state define the number of parameters based on the size of the state spaces of the parent and child variables.

The `custom-score` object has an initialization keyword argument `:penalty-fn` that can be used to define any desired penalty function.


### k-Groups

k-groups are used for evaluating heuristics in A* search. Acyclicity is asserted between between the elements of each k-group. For example, a single k-group with all variables essentially causes the heuristic to perform full combinatorial search, while putting each variable in its own k-group does no acyclicity checks in the heuristic.

Smaller k-groups are easier to compute the heuristic for, but larger k-groups result in a more accurate heuristic value. See the paper "Yuan and Malone, Learning Optimal Bayesian Networks: A Shortest Path Perspective" for a discussion on choices when using A* without bounding conflicts.

In A*BC, we have found that smaller k-groups tend to perform better. Enforcing acyclicity in the heuristic tends to leads to optimal parent sets that have not had their likelihoods evaluated, actually resulting in weaker heuristic values. 


### Search

This package implements A* search, A\*BC search, and tabu search, though the main focus of the package is on A\*BC.

The `abc-search` method has the following signature:

```
(abc-search initial-node score log-lik-fn k-groups
            &optional candidate-dag allowable-fn max-set-fn)
```

- `initial-node`: Initial search node for A*BC.
- `score`: A score object determining how to penalize DAG complexity.
- `log-lik-fn`: A functtion `(log-lik-fn var parents) => log-lik` that returns the optimized log likelihood of var with given parents.
- `k-groups`: A list of lists of variable indices, defining the k-groups for heuristic computation.
- `candidate-dag`: A guess of the optimal DAG. When specified, compute the likelihoods of this DAG prior to performing search. If this is close to the optimal DAG, then bounds for other variables will likely be stronger.
- `allowable-fn`: Used to restrict the space of possible DAG structures. If specified, must be a function `(allowable-fn var parents)`. If this function returns non-nil, the parents given by `parents` are permitted. If not, the parents are not a permissible set.
- `max-set-fn`: Used when restricting the space of possible DAG structures. If specified, must be a function `(max-set-fn var) => parents`. The parent set returned by this function is evaluated at the start of A*BC to provide bounds on all allowable parent sets, so should be the smallest set that is a superset of all parent sets for which `allowable-fn` returns non-nil. If not specified, the set of all other outputs in the DAG is used.


The `astar-search` method has the following signature:

```
(astar-search initial-node score supplied-log-liks k-groups
              &optional allowable-fn)
```

- `supplied-log-liks`: An a-list from variables to hash tables, each hash table points from parents to log likelihoods. Parents are assumed to be sorted from lowest to highest. All log likelihoods to be used during search must be present. If A* search attempts to access an undefined log-likelihood, and error will result.

All other arguments remain the same as `abc-search`.


The `tabu-search` method has the following signature:

```
(tabu-search log-lik-fn 
             &key :candidate-dag
                  :max-restarts
                  :max-iters
                  :reach-dag
                  :allowable-fn)
```

- `:candidate-dag` (default `nil`): A guess of the optimal DAG. When non-nil, used as the initial guess for tabu search. If nil, the empty DAG is used instead.
- `:max-restarts` (default 5): The number of times to restart search from a new random DAG.
- `:max-iters` (default 50): The number of iterations per restart in local search. At each iteration, all local transformations are evaluated, and the best transformation is greedily selected.
- `:reach-dag` (default `nil`): When non-nil, the algorithm will terminate as soon as the supplied DAG is selected as the best DAG. This is useful for measuring the exact number of iterations to reach a known optimal DAG.

All other arguments remain the same as `abc-search`.




