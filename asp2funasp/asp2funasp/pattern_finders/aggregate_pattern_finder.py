from typing import Iterable, List

from clingo import ast
from clingo.core import Library

from asp2funasp.pattern_finders.pattern_finder_utils import (
    predicate_key_from_literal_symbolic,
    split_program,
)
from asp2funasp.util.ast import StatementAST
from asp2funasp.util.types import CPredicate, FPredicate
from asp2funasp.util.util import collect_statements_from_pased, get_parameter_list


class AggregatePatternFinder:
    def __init__(self, library: Library) -> None:
        self.library = library
        self.constraints: list[ast.StatementRule]
        self.predicate_definitions: dict[str, list[ast.StatementRule]]
        self.functionalPredicates: list[FPredicate] = []

    def identifyAggregatePattern(
        self, statements: Iterable[StatementAST]
    ) -> List[FPredicate]:
        """
        Identifies functional relationships in aggregate rules by checking for the pattern:
          - Only one predicate on the left of the colon in the aggregate head.
          - Either left or right (or both) equals 1.
        Uses the main_class.clingoParseString module with a local callback function.
        """
        statements = collect_statements_from_pased(statements)
        self.constraints, self.predicate_definitions = split_program(statements)
        for rule in statements:
            if isinstance(rule, ast.StatementRule):
                # print(
                #     {
                #         key: [str(rule) for rule in rules]
                #         for key, rules in self.predicate_definitions.items()
                #     }
                # )
                # Check if the head is an aggregate
                if isinstance(rule.head, ast.HeadAggregate):
                    # Ignore aggregates that have more than one predicate on the left side of the colon.
                    if len(rule.head.elements) == 1:
                        # Retrieve the left side (the choice part) and the conditions (after the colon)
                        choice_literal = rule.head.elements[0].literal
                        conditions = rule.head.elements[0].condition

                        # Check for functional guard: either left or right equals 1.
                        left = rule.head.left
                        right = rule.head.right
                        if left or right:
                            # if either exists, check if their operator is equality and the numeric term is 1.
                            if (left is not None and self.guardCheck(left)) or (
                                right is not None and self.guardCheck(right)
                            ):

                                # Check if the choice literal is defined by any other rule or not.
                                choiceKey = (
                                    predicate_key_from_literal_symbolic(choice_literal)
                                    if isinstance(choice_literal, ast.LiteralSymbolic)
                                    else None
                                )

                                if (
                                    choiceKey
                                    and choiceKey in self.predicate_definitions
                                ):
                                    if (
                                        len(self.predicate_definitions[choiceKey]) == 1
                                        and rule
                                        in self.predicate_definitions[choiceKey]
                                    ):
                                        # Proceed further if this is the only rule that defines the choiceLiteral

                                        choice_parameters = get_parameter_list(
                                            choice_literal
                                        )  # atom.symbol if ASTType.Function
                                        # print(
                                        #     "Choice literal parameters:",
                                        #     choice_parameters,
                                        # )

                                        condition_parameters = []
                                        for condition in conditions:
                                            params = get_parameter_list(condition)
                                            condition_parameters.extend(params)
                                        # print(
                                        #     "Condition parameters:",
                                        #     condition_parameters,
                                        # )

                                        body_parameters = []
                                        for body_element in rule.body:
                                            # Assuming the body contains only literals
                                            if isinstance(
                                                body_element, ast.BodySimpleLiteral
                                            ):
                                                literal = body_element.literal
                                                params = get_parameter_list(literal)
                                                body_parameters.extend(params)
                                        # print("Body parameters:", body_parameters)

                                        argumentList = []
                                        returnValueList = []
                                        for index, param in enumerate(
                                            choice_parameters
                                        ):
                                            # If the parameter appears in the body parameters, add its index to the argument list.
                                            if param in body_parameters:
                                                argumentList.append(index)
                                            # If the parameter appears in the condition parameters, add its index to the return value list.
                                            if param in condition_parameters:
                                                returnValueList.append(index)

                                        all_indices = set(
                                            argumentList + returnValueList
                                        )
                                        if (
                                            len(all_indices) == len(choice_parameters)
                                            and len(
                                                set(argumentList).intersection(
                                                    set(returnValueList)
                                                )
                                            )
                                            == 0
                                        ):
                                            fpredicate = FPredicate(
                                                name=choiceKey.split("/")[0],
                                                arity=len(choice_parameters),
                                                arguments=tuple(argumentList),
                                                values=tuple(returnValueList),
                                                condition=[],
                                            )

                                            # Append to the class's list of functional predicates
                                            self.functionalPredicates.append(fpredicate)

        return self.functionalPredicates

    # def identifyCountConstraintPattern(
    #     self, statements: Iterable[StatementAST]
    # ) -> List[FPredicate]:
    #     """
    #     Identifies the target pattern from constraints only.

    #     Target Pattern (example):
    #       { assign(N,C) } :- node(N), color(C), ...
    #       :- #count{ C,N : assign(N,C) } != 1, node(N).

    #     The idea:
    #       1. In constraints (reconstructed from main_class.constraints), look for a body aggregate
    #          (#count aggregate) with guard '!= 1'.
    #       2. For such a constraint, extract the predicate from the aggregate body.
    #       3. Use util.pred_to_key on that predicate to check main_class.definitions.
    #          Proceed only if exactly one (choice?) rule defines that predicate.
    #       4. Extract literal sets from both the constraint's body (non-aggregate parts) and the
    #          defining rule's body. Ensure the constraint's literal set is a subset of the defining rule's.
    #       5. Finally, using getParameterList(), extract parameters from:
    #          - The choice literal of the defining rule (the functional predicate).
    #          - The constraint body literals.
    #       6. For each parameter in the functional predicate's parameter list, if it appears in the
    #          constraint body, record its index as an argument; otherwise, record it as a return value.
    #       7. If all parameters are accounted for, create an FPredicate namedtuple and append it to
    #          main_class.functionalPredicates.
    #     """
    #     statements = collect_statements_from_pased(statements)
    #     self.constraints, self.predicate_definitions = split_program(statements)
    #     for rule in self.constraints:
    #         for bodyElement in rule.body:
    #             if isinstance(bodyElement, ast.BodyAggregate):
    #                 pass
    #     return []

    # def identifyCountConstraintPattern(self):
    #     """
    #     Identifies the target pattern from constraints only.

    #     Target Pattern (example):
    #       { assign(N,C) } :- node(N), color(C), ...
    #       :- #count{ C,N : assign(N,C) } != 1, node(N).

    #     The idea:
    #       1. In constraints (reconstructed from main_class.constraints), look for a body aggregate
    #          (#count aggregate) with guard '!= 1'.
    #       2. For such a constraint, extract the predicate from the aggregate body.
    #       3. Use util.pred_to_key on that predicate to check main_class.definitions.
    #          Proceed only if exactly one (choice?) rule defines that predicate.
    #       4. Extract literal sets from both the constraint's body (non-aggregate parts) and the
    #          defining rule's body. Ensure the constraint's literal set is a subset of the defining rule's.
    #       5. Finally, using getParameterList(), extract parameters from:
    #          - The choice literal of the defining rule (the functional predicate).
    #          - The constraint body literals.
    #       6. For each parameter in the functional predicate's parameter list, if it appears in the
    #          constraint body, record its index as an argument; otherwise, record it as a return value.
    #       7. If all parameters are accounted for, create an FPredicate namedtuple and append it to
    #          main_class.functionalPredicates.
    #     """
    #     # This list will collect new FPredicate objects.
    #     for rule in self.main_class.asp_program_ast:
    #         try:
    #             if rule.ast_type == ast.ASTType.Rule:
    #                 # Only process constraints
    #                 if rule.head.ast_type == ast.ASTType.Literal and rule.head.atom.ast_type == ast.ASTType.BooleanConstant and rule.head.sign == 0:
    #                     # Checking for a body aggregate literal with a '#count' operator and guard '!= 1'.
    #                     for literal in rule.body:
    #                         if literal.ast_type == ast.ASTType.Literal and literal.atom.ast_type == ast.ASTType.BodyAggregate:
    #                             bodyAggregate = literal.atom
    #                             # Checking left is "!= 1" or not.
    #                             # Checking left guard only since left guard exists in target pattern after preprocessing.
    #                             if bodyAggregate.left is not None:
    #                                 if self.guardCheck(bodyAggregate.left, operator=ast.ComparisonOperator.NotEqual, num=1):
    #                                     # Extract the predicate key from the aggregate conditions.
    #                                     # ASSUMPTION: Assume the aggregate has only one element.
    #                                     bodyAggregateElement = bodyAggregate.elements[0]

    #                                     choiceLiteral = bodyAggregateElement.condition[0].atom
    #                                     choiceKey = util.pred_to_str(choiceLiteral)

    #                                     # Check if the predicate is defined by exactly one rule.
    #                                     if choiceKey in self.main_class.definitions and len(self.main_class.definitions[choiceKey]) == 1:
    #                                         # Get the defining rule string, maybe relevant later
    #                                         definingRuleStr = self.main_class.definitions[choiceKey][0]

    #                                         # Extract condition literal set from the defining rule’s body and the constraint body.
    #                                         definingLiterals = self.getBodyLiterals(definingRuleStr)
    #                                         constraintLiterals = self.getBodyLiterals(rule)

    #                                         # Check if the conditions from constraint all appear in the body
    #                                         if set(constraintLiterals).issubset(set(definingLiterals)):
    #                                             # Extract parameters of literal set from constraint body (ignoring the aggregate literal).
    #                                             constraintBodyParams = []
    #                                             for lit in rule.body:
    #                                                 # Skip aggregates
    #                                                 if not (lit.ast_type == ast.ASTType.Literal and lit.atom.ast_type == ast.ASTType.BodyAggregate):
    #                                                     constraintBodyParams.extend(get_parameter_list(lit))
    #                                             # print(f"Constraint body parameters: {constraintBodyParams}")

    #                                             # Extract parameters from the functional predicate (choice literal) in the defining rule.
    #                                             # The choice literal is assumed to be in the aggregate head of the defining rule.
    #                                             # For example, { assign(N,C) } :- ...
    #                                             # Extract the parameters from that choice literal.
    #                                             choiceLiteralParams = get_parameter_list(choiceLiteral)
    #                                             # print(f"Choice literal parameters: {choiceLiteralParams}")

    #                                             # Extracts C and N from the aggregate body.
    #                                             aggregateHeadVars = [term.name for term in bodyAggregateElement.terms]
    #                                             # print(f"Aggregate Head Variables: {aggregateHeadVars}")

    #                                             argumentList = []
    #                                             returnValueList = []

    #                                             # Iterate over the choice literal parameters with their index.
    #                                             for idx, param in enumerate(choiceLiteralParams):
    #                                                 if param in constraintBodyParams:
    #                                                     argumentList.append(idx)
    #                                                 elif param in aggregateHeadVars:
    #                                                     returnValueList.append(idx)

    #                                             # Validation: Ensure that every parameter in the choice literal is accounted for.
    #                                             # Validation: Ensure no extra variables are present on the left of the colon in the aggregate.
    #                                             all_indices = set(argumentList + returnValueList)
    #                                             if len(all_indices) == len(choiceLiteralParams) and set(aggregateHeadVars).issubset(set(choiceLiteralParams)) and len(argumentList)>0 and len(returnValueList)>0:
    #                                                 fpredicate = FPredicate(
    #                                                             name=choiceKey.split("/")[0],
    #                                                             arity=len(choiceLiteralParams),
    #                                                             arguments=tuple(argumentList),
    #                                                             values=tuple(returnValueList),
    #                                                             condition=[]
    #                                                         )
    #                                                 self.main_class.functionalPredicates.append(fpredicate)

    #         except Exception as e:
    #             print("Error in count_constraint_callback:", e)

    #             import traceback
    #             print(traceback.format_exc())

    #     pass

    def guardCheck(
        self,
        guard: ast.LeftGuard | ast.RightGuard,
        operator: ast.Relation = ast.Relation.Equal,
        num: int = 1,
    ) -> bool:
        """
        Checks if the given guard has same operator and number as passed.
        This helper function should check that:
          - The guard's comparison operator is '=' (or operator),
          - The guard's term (after conversion) equals num.

        returns:
        bool: True if the guard is equal to num with the supplied comparison, False otherwise.
        """
        if guard.relation == operator:
            if isinstance(guard.term, ast.TermSymbolic):
                if guard.term.symbol.number == num:
                    return True
        return False

    # def getBodyLiterals(self, rule):
    #     """
    #     Extracts all non-aggregate literals from the body of a rule.
    #     Returns:
    #     List: String representations (or a normalized form) of the literals.
    #     """
    #     def _getBodyLiterals(rule):
    #         if rule.ast_type == ast.ASTType.Rule:
    #             for literal in rule.body:
    #                 # Only append literals having of atom type SymbolicAtoms .
    #                 if literal.atom.ast_type == ast.ASTType.SymbolicAtom:
    #                     # literals.append(str(literal))
    #                     literals.append(literal)
    #     literals = []
    #     if type(rule) != ast.AST:
    #         tempProgram = str(rule)
    #         self.main_class.clingoParseString(program=tempProgram, callback=_getBodyLiterals )
    #     else:
    #         for literal in rule.body:
    #         # Only append literals having of atom type SymbolicAtoms .
    #             if literal.atom.ast_type == ast.ASTType.SymbolicAtom:
    #                 # literals.append(str(literal))
    #                 literals.append(literal)
    #     return literals
