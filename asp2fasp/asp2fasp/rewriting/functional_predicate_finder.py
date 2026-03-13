from typing import Any, Dict, Iterable, List, Tuple
from collections import defaultdict

from clingo import ast
from clingo.core import Library

import asp2fasp.util.util as util
from asp2fasp.transformers.preprocessing import processPipelinetransformers
from asp2fasp.util.ast import StatementAST
from asp2fasp.util.types import CPredicate, FPredicate, FRelation

from asp2fasp.pattern_finders import InequalityConstraintFinder


class FunctionalPredicateFinder:
    def __init__(self, library: Library) -> None:
        self.lib = library
        self.statements:Iterable[StatementAST] = []
        self.functionalPredicates: List[FPredicate] = []
        self.functionalRelations: List[FRelation] = []
        self.constraints: List[StatementAST] = []
        self.definitions: Dict[str, Any] = {}

    # def getFunctionalPredicates(self) -> List[FPredicate]:
    #     return self.functionalPredicates

    # def foundFunctionalPredicate(self) -> bool:
    #     return len(self.functionalPredicates) > 0

    # def getFunctionalRelations(self) -> List[FRelation]:
    #     return self.functionalRelations

    # def foundFunctionalRelation(self) -> bool:
    #     return len(self.functionalRelations) > 0


# def update_program_string(self):
#     rule_strings = []
#     for node in self.statements:
#         rule_strings.append(str(node))
#     self.asp_program = util.clean_program_base("\n".join(rule_strings), removeDirective=True)
#     return self.asp_program


    def processProgram(self, statements:List[StatementAST] | List[ast.StatementRule]) -> Tuple[List[FPredicate], List[FRelation]]:
        # # Preprocess the program with the pipeline of transformers, which includes splitting rules with multiple aggregate elements
        # self.statements = processPipelinetransformers(self.lib, statements)

        self.statements = statements
        
        foundFunctionalPredicates:List[FPredicate] = []
        # Identify functional predicates patterns
        ICF = InequalityConstraintFinder(self.lib)
        foundFunctionalPredicates = ICF.identifyInequalityPattern(self.statements)

        self.functionalPredicates.extend(foundFunctionalPredicates)
    
        return self.functionalPredicates, self.processFoundPredicates()



# if self.asp_program:
#     APF = AggregatePatternFinder(self)
#     APF.identifyAggregatePattern()
#     APF.identifyCountConstraintPattern()
#     ICF = InequalityConstraintFinder(self)
#     ICF.identifyInequalityPattern()
#     self.processFoundPredicates()
#     return self.asp_program

    def processFoundPredicates(self) -> List[FRelation]:
        groupedPredicates = defaultdict(list)
        for fPredicate in self.functionalPredicates:
            if fPredicate.condition == []:
                key = (fPredicate.name, fPredicate.arguments, fPredicate.arity)
                groupedPredicates[key].append(fPredicate.values)
        for (name, arguments, arity), valuesList in groupedPredicates.items():
            fullIndices = set(range(arity))
            remainingIndices = fullIndices - set(arguments)
            extractedValues = set()
            valuesTuples = []
            for values in valuesList:
                valuesTuples.append(tuple(values))
                extractedValues.update(values)
            if extractedValues == remainingIndices:
                self.functionalRelations.append(FRelation(name, arity, arguments, valuesTuples))
        return self.functionalRelations

# def rewriteFunctionalRelations(self):
#     new_program = []
#     def rewrite_callback(ast_rule):
#         if not isinstance(ast_rule, ast.StatementRule):
#             return
#         head = ast_rule.head
#         try:
#             if isinstance(head, ast.LiteralSymbolic) and isinstance(head.atom, ast.AtomSymbolic):
#                 fPred = head.atom.symbol
#                 name = fPred.name
#                 arity = len(fPred.arguments)
#                 for relation in self.functionalRelations:
#                     if relation.name == name and relation.arity == arity and len(relation.values) > 1:
#                         orig_params = [arg.name for arg in fPred.arguments]
#                         for val_tuple in relation.values:
#                             new_name = f"{name}_{str(val_tuple).replace('(', '').replace(')', '').replace(',', '')}"
#                             head_args = [orig_params[a] for a in relation.arguments]
#                             for vpos in val_tuple:
#                                 head_args.append(orig_params[vpos])
#                             head_str = f"{new_name}({','.join(head_args)})"
#                             body_strs = []
#                             for literal in ast_rule.body:
#                                 if isinstance(literal, ast.LiteralSymbolic) and isinstance(literal.atom, ast.AtomSymbolic):
#                                     bfPred = literal.atom.symbol
#                                     if bfPred.name == name and len(bfPred.arguments) == arity:
#                                         bf_params = [arg.name for arg in bfPred.arguments]
#                                         new_body_args = [bf_params[a] for a in relation.arguments]
#                                         for vpos in val_tuple:
#                                             new_body_args.append(bf_params[vpos])
#                                         body_strs.append(f"{new_name}({','.join(new_body_args)})")
#                                         continue
#                                 body_strs.append(str(literal))
#                             new_program.append(f"{head_str} :- {', '.join(body_strs)}.")
#                         return
#         except Exception as e:
#             print(f"Error rewriting functional relation: {e}")
#             import traceback
#             print(traceback.format_exc())
#         new_program.append(str(ast_rule))
#     self.clingoParseString(self.asp_program, rewrite_callback)
#     self.asp_program = "\n".join(new_program)
#     return self.asp_program

# def splitProgram(self, program=None):
#     if program is None:
#         program = self.asp_program
#     constraints = []
#     definitions = defaultdict(list)
#     def process_rule(rule):
#         if isinstance(rule, ast.StatementRule):
#             try:
#                 head = rule.head
#                 # Constraints
#                 if (isinstance(head, ast.LiteralBoolean) and head.sign == 0):
#                     constraints.append(str(rule))
#                 # Fact rules
#                 elif len(rule.body) == 0:
#                     key = util.pred_to_str(head.atom)
#                     definitions[key].append(str(rule))
#                 # Rules with one head Literal
#                 elif isinstance(head, ast.LiteralSymbolic):
#                     key = util.pred_to_str(head.atom)
#                     definitions[key].append(str(rule))
#                 # Rules with disjunction in head
#                 elif isinstance(head, ast.HeadDisjunction):
#                     for elem in head.elements:
#                         if isinstance(elem.literal.atom, ast.AtomSymbolic):
#                             key = None
#                             if isinstance(elem.literal.atom.symbol, ast.TermUnaryOperation):
#                                 key = util.pred_to_str(elem.literal.atom, uniaryOp=True)
#                             else:
#                                 key = util.pred_to_str(elem.literal.atom)
#                             definitions[key].append(str(rule))
#                 # Rules with aggregate in head
#                 elif isinstance(head, ast.HeadAggregate):
#                     for elem in head.elements:
#                         key = util.pred_to_str(elem.literal.atom)
#                         definitions[key].append(str(rule))
#                 else:
#                     print(f"Rule head is of type : {type(head)}")
#                     print(f"Rule: {rule}")
#             except Exception as e:
#                 print(f"Error Splitting Program: {e}")
#                 print(f"Rule: {rule}")
#                 import traceback
#                 print(traceback.format_exc())
#     ast.parse_string(self.lib, program, process_rule)
#     self.constraints = list(constraints)
#     self.definitions = dict(definitions)
#     return self.constraints, self.definitions

# def reConstructProgram(self, updateProgram=True, constraints=None, definitions=None):
#     if constraints is None:
#         constraints = self.constraints
#     if definitions is None:
#         definitions = self.definitions
#     new_program_rules = []
#     seen_rules = set()
#     for constraint in constraints:
#         if constraint not in seen_rules:
#             new_program_rules.append(constraint)
#             seen_rules.add(constraint)
#     for key, rules in definitions.items():
#         for rule in rules:
#             if rule not in seen_rules:
#                 new_program_rules.append(rule)
#                 seen_rules.add(rule)
#     reconstructedProgram = "\n".join(new_program_rules)
#     if updateProgram:
#         self.asp_program = reconstructedProgram
#     return reconstructedProgram
