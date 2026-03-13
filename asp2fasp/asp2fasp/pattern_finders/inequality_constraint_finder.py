from typing import Iterable, List, Sequence, Tuple, TypedDict, cast

import clingo.ast as ast

import asp2fasp.util.util as util
from asp2fasp.util.ast import AST, StatementAST, TermAST
from asp2fasp.util.types import CPredicate, FPredicate


class FunctionalPredicateData(TypedDict):
    inputs: set[int]
    outputs: set[int]


class InequalityConstraintFinder:
    def __init__(self) -> None:
        self.comparisons: list[ast.LiteralComparison] = []
        self.symbolicAtoms: list[ast.LiteralSymbolic] = []
        self.occurrences: list[int] = []
        self.argumentIndices: list[int] = []
        self.functional_predicates: dict[str, FunctionalPredicateData] = {}
        self.conditions: list[ast.LiteralComparison] = []
        self.foundPredicates: list[FPredicate] = []

    def identifyInequalityPattern(
        self, statements: Iterable[StatementAST]
    ) -> List[FPredicate]:
        """
        Identifies functional relationships in constraints by checking for the pattern for
        functional relations.  Stores found relations as FPredicate tuples.
        """
        if self.foundPredicates:
            self.foundPredicates.clear()
        for rule in statements:
            if isinstance(rule, ast.StatementRule):
                # Only analyze if rule is a constraint.
                if not util.is_constraint(rule.head):
                    continue

                # Clear values from previous rules, if any.
                self.symbolicAtoms = []
                self.comparisons = []

                # Capture ast.LiteralSymbolic and ast.LiteralComparison from ast.BodySimpleLiteral in rule.body
                for body_elem in rule.body:
                    if isinstance(body_elem, ast.BodySimpleLiteral):
                        # Dispatch on the inner literal type.
                        if isinstance(body_elem.literal, ast.LiteralSymbolic):
                            # Store the LiteralSymbolic; .atom is the term directly.
                            self.symbolicAtoms.append(body_elem.literal)
                        elif isinstance(body_elem.literal, ast.LiteralComparison):
                            self.comparisons.append(body_elem.literal)
                self.mapAndCheckPredicates()
        return self.foundPredicates

    def _map_predicate_occurrences(
        self, symbolicAtoms: List[ast.LiteralSymbolic]
    ) -> dict[str, List[Sequence[ast.TermOrProjection]]]:
        predicates: dict[str, List[Sequence[ast.TermOrProjection]]] = {}
        for sym_lit in symbolicAtoms:
            term = sym_lit.atom

            if isinstance(term, ast.TermFunction):
                name, args = util.function_arguments(term)
                term_args = cast(Sequence[ast.TermOrProjection], args)

                if name not in predicates:
                    predicates[name] = []

                predicates[name].append(term_args)
        return predicates

    def _term_key(self, term: AST) -> str:
        if isinstance(term, ast.TermVariable):
            return term.name
        return str(term)  # pragma: no cover

    def mapAndCheckPredicates(self) -> None:
        # Clear values from previous rules, if any.
        self.occurrences = []
        self.argumentIndices = []
        self.functional_predicates = {}
        self.conditions = []

        predicates: dict[str, List[Sequence[ast.TermOrProjection]]] = (
            self._map_predicate_occurrences(self.symbolicAtoms)
        )

        # Check for comparisons in predicates with multiple occurrences in the same rule.
        for name, occurrences in predicates.items():
            if len(occurrences) > 1:
                invariantArgs = util.identify_invariant_positions(occurrences)
                if 0 in invariantArgs and 1 in invariantArgs:
                    self.identify_functional_arguments(name, occurrences, invariantArgs)
                    if len(self.functional_predicates) > 0:
                        foundPredicates = [
                            FPredicate(
                                name=predicate,
                                arity=len(predicates[predicate][0]),
                                arguments=tuple(
                                    self.functional_predicates[predicate]["inputs"]
                                ),
                                values=tuple(
                                    self.functional_predicates[predicate]["outputs"]
                                ),
                                condition=self.getConditionPredicates(
                                    predicate, predicates[predicate]
                                ),
                            )
                            for predicate in self.functional_predicates.keys()
                        ]
                        for fPredicate in foundPredicates:
                            self.foundPredicates.append(fPredicate)

    def identify_functional_arguments(
        self,
        name: str,
        occurrences: Sequence[Sequence[ast.TermOrProjection]],
        invariant_mask: List[int],
    ) -> None:
        variant_positions = [i for i, v in enumerate(invariant_mask) if v == 0]
        input_positions = {i for i, v in enumerate(invariant_mask) if v == 1}

        candidate_subset_indices: List[Tuple[int, ...]] = []

        subsets = util.get_variant_subsets(variant_positions, occurrences)
        for idx, (subset_values0, subset_indices) in enumerate(subsets[0]):
            set0 = {self._term_key(term) for term in subset_values0}

            for comparison in self.comparisons:
                # extract_comparison_terms handles clingo 6 LiteralComparison;
                # operator is now ast.Relation (renamed from ast.ComparisonOperator in clingo 5).
                raw_lhs_terms, operator, raw_rhs_terms = util.extract_comparison_terms(
                    comparison
                )

                lhs_terms, rhs_terms = self._extract_comparison_terms_from_term_tuples(
                    raw_lhs_terms, raw_rhs_terms
                )
                if operator != ast.Relation.NotEqual:
                    continue  # Only consider inequality constraints.

                lhs_set = set()
                rhs_set = set()
                for term in lhs_terms:
                    lhs_set.add(self._term_key(term))
                for term in rhs_terms:
                    rhs_set.add(self._term_key(term))

                # Check if the candidate subset from occurrence 0 matches one side.
                if set0 == lhs_set:
                    # Then the corresponding candidate from occurrence 1 must match the other side.
                    subset_values1, _ = subsets[1][idx]
                    set1 = {self._term_key(term) for term in subset_values1}
                    if set1 == rhs_set:
                        candidate_subset_indices.append(tuple(subset_indices))
                elif set0 == rhs_set:
                    subset_values1, _ = subsets[1][idx]
                    set1 = {self._term_key(term) for term in subset_values1}
                    if set1 == lhs_set:
                        candidate_subset_indices.append(tuple(subset_indices))

        # If exactly one candidate subset is found, accept it as the functional output.
        if len(candidate_subset_indices) == 1:
            chosen_variant_indices = candidate_subset_indices[0]
            if name not in self.functional_predicates:
                self.functional_predicates[name] = {
                    "inputs": set[int](),
                    "outputs": set[int](),
                }
            self.functional_predicates[name]["inputs"].update(input_positions)
            self.functional_predicates[name]["outputs"].update(chosen_variant_indices)
        # else: ambiguous, skip.

    def getConditionPredicates(
        self, _name: str, _occurrences: Sequence[Sequence[ast.TermOrProjection]]
    ) -> List[CPredicate]:
        conditionPredicates: List[CPredicate] = []
        # NOTE: Assumes mapAndCheckPredicates has already processed the first two
        # symbolicAtoms for identifying the functional relation.
        countSymbolicAtom = 0
        for sym_lit in self.symbolicAtoms:
            term = sym_lit.atom

            if isinstance(term, ast.TermFunction):
                name = term.name

                if name != _name:
                    _, args = util.function_arguments(term)
                    conditionArgs = list(args)
                    # Note: Index in the functional argument for the corresponding condition argument.
                    sharedArgsIndices = []
                    for arg in conditionArgs:
                        for occurrence in _occurrences:
                            if arg in occurrence:
                                sharedArgsIndices.append(occurrence.index(arg))
                            else:
                                sharedArgsIndices.append(-1)
                            break

                    conditionPredicate = CPredicate(
                        name=name, arity=len(args), arguments=tuple(sharedArgsIndices)
                    )
                    conditionPredicates.append(conditionPredicate)

                # Case: conditional functional relation with more than 2 occurrences of the same predicate.
                elif name == name and len(_occurrences) >= 2:
                    if countSymbolicAtom >= 2:
                        _, args = util.function_arguments(term)
                        conditionArgs = list(args)
                        sharedArgsIndices = []
                        for arg in conditionArgs:
                            for occurrence in _occurrences:
                                if arg in occurrence:
                                    sharedArgsIndices.append(occurrence.index(arg))
                                else:
                                    sharedArgsIndices.append(-1)
                                break

                        conditionPredicate = CPredicate(
                            name=name,
                            arity=len(args),
                            arguments=tuple(sharedArgsIndices),
                        )
                        conditionPredicates.append(conditionPredicate)
                    else:
                        countSymbolicAtom += 1

        return conditionPredicates

    # TODO: Find a cleaner way to extract terms from comparisons, handling the case where they are wrapped in TermTuples.
    def _extract_comparison_terms_from_term_tuples(
        self, lhs_terms: Iterable[TermAST], rhs_terms: Iterable[TermAST]
    ) -> Tuple[Sequence[AST], Sequence[AST]]:

        lhs_seq: Sequence[AST] = list(lhs_terms)
        rhs_seq: Sequence[AST] = list(rhs_terms)

        if (
            lhs_seq
            and isinstance(lhs_seq[0], ast.TermTuple)
            and lhs_seq[0].pool
            and isinstance(lhs_seq[0].pool[0], ast.ArgumentTuple)
        ):
            lhs_seq = cast(Sequence[AST], lhs_seq[0].pool[0].arguments)
        if (
            rhs_seq
            and isinstance(rhs_seq[0], ast.TermTuple)
            and rhs_seq[0].pool
            and isinstance(rhs_seq[0].pool[0], ast.ArgumentTuple)
        ):
            rhs_seq = cast(Sequence[AST], rhs_seq[0].pool[0].arguments)
        return lhs_seq, rhs_seq
