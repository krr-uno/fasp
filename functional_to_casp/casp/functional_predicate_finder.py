# import functional_to_casp.casp.util as util
# import clingo
# import clingo.core
# import clingo.ast as ast
# from collections import namedtuple, defaultdict
# from functional_to_casp.casp.patternFinders.ast_utils import ASTUtils
# from functional_to_casp.casp.patternFinders import (InequalityConstraintFinder, AggregatePatternFinder)

# FPredicate = namedtuple('FPredicate', ['name', 'arity', 'arguments', 'values', 'condition'])
# CPredicate = namedtuple('CPredicate', ['name', 'arity', 'arguments'])
# FRelation = namedtuple('FRelation', ['name', 'arity', 'arguments', 'values'])

# class FunctionalPredicateFinder6:
#     def __init__(self, _asp_program=""):
#         self.asp_program = _asp_program
#         self.asp_program_ast = []
#         self.functionalPredicates = []
#         self.functionalRelations = []
#         self.constraints = []
#         self.definitions = {}
#         self.lib = clingo.core.Library()
#         ast.parse_string(self.lib, self.asp_program, self.asp_program_ast.append)

#     def getFunctionalPredicates(self):
#         return self.functionalPredicates

#     def foundFunctionalPredicate(self):
#         return len(self.functionalPredicates) > 0

#     def getFunctionalRelations(self):
#         return self.functionalRelations

#     def foundFunctionalRelation(self):
#         return len(self.functionalRelations) > 0

#     def clingoParseString(self, program=None, callback=None):
#         if program is None:
#             program = self.asp_program
#         if callback is None:
#             callback = self.visit
#         ast.parse_string(self.lib, program, callback)

#     def update_program_string(self):
#         rule_strings = []
#         for node in self.asp_program_ast:
#             rule_strings.append(str(node))
#         self.asp_program = util.clean_program_base("\n".join(rule_strings), removeDirective=True)
#         return self.asp_program

#     def visit(self, node):
#         if isinstance(node, ast.StatementRule):
#             print(node)

#     def processProgram(self):
#         if self.asp_program:
#             APF = AggregatePatternFinder(self)
#             APF.identifyAggregatePattern()
#             APF.identifyCountConstraintPattern()
#             ICF = InequalityConstraintFinder(self)
#             ICF.identifyInequalityPattern()
#             self.processFoundPredicates()
#             return self.asp_program

#     def processFoundPredicates(self):
#         groupedPredicates = defaultdict(list)
#         for fPredicate in self.functionalPredicates:
#             if fPredicate.condition == []:
#                 key = (fPredicate.name, fPredicate.arguments, fPredicate.arity)
#                 groupedPredicates[key].append(fPredicate.values)
#         for (name, arguments, arity), valuesList in groupedPredicates.items():
#             fullIndices = set(range(arity))
#             remainingIndices = fullIndices - set(arguments)
#             extractedValues = set()
#             valuesTuples = []
#             for values in valuesList:
#                 valuesTuples.append(tuple(values))
#                 extractedValues.update(values)
#             if extractedValues == remainingIndices:
#                 self.functionalRelations.append(FRelation(name, arity, arguments, valuesTuples))
#         return self.functionalRelations

#     def rewriteFunctionalRelations(self):
#         new_program = []
#         def rewrite_callback(ast_rule):
#             if not isinstance(ast_rule, ast.StatementRule):
#                 return
#             head = ast_rule.head
#             try:
#                 if isinstance(head, ast.LiteralSymbolic) and isinstance(head.atom, ast.AtomSymbolic):
#                     fPred = head.atom.symbol
#                     name = fPred.name
#                     arity = len(fPred.arguments)
#                     for relation in self.functionalRelations:
#                         if relation.name == name and relation.arity == arity and len(relation.values) > 1:
#                             orig_params = [arg.name for arg in fPred.arguments]
#                             for val_tuple in relation.values:
#                                 new_name = f"{name}_{str(val_tuple).replace('(', '').replace(')', '').replace(',', '')}"
#                                 head_args = [orig_params[a] for a in relation.arguments]
#                                 for vpos in val_tuple:
#                                     head_args.append(orig_params[vpos])
#                                 head_str = f"{new_name}({','.join(head_args)})"
#                                 body_strs = []
#                                 for literal in ast_rule.body:
#                                     if isinstance(literal, ast.LiteralSymbolic) and isinstance(literal.atom, ast.AtomSymbolic):
#                                         bfPred = literal.atom.symbol
#                                         if bfPred.name == name and len(bfPred.arguments) == arity:
#                                             bf_params = [arg.name for arg in bfPred.arguments]
#                                             new_body_args = [bf_params[a] for a in relation.arguments]
#                                             for vpos in val_tuple:
#                                                 new_body_args.append(bf_params[vpos])
#                                             body_strs.append(f"{new_name}({','.join(new_body_args)})")
#                                             continue
#                                     body_strs.append(str(literal))
#                                 new_program.append(f"{head_str} :- {', '.join(body_strs)}.")
#                             return
#             except Exception as e:
#                 print(f"Error rewriting functional relation: {e}")
#                 import traceback
#                 print(traceback.format_exc())
#             new_program.append(str(ast_rule))
#         self.clingoParseString(self.asp_program, rewrite_callback)
#         self.asp_program = "\n".join(new_program)
#         return self.asp_program

#     def splitProgram(self, program=None):
#         if program is None:
#             program = self.asp_program
#         constraints = []
#         definitions = defaultdict(list)
#         def process_rule(rule):
#             if isinstance(rule, ast.StatementRule):
#                 try:
#                     head = rule.head
#                     # Constraints
#                     if (isinstance(head, ast.LiteralBoolean) and head.sign == 0):
#                         constraints.append(str(rule))
#                     # Fact rules
#                     elif len(rule.body) == 0:
#                         key = util.pred_to_str(head.atom)
#                         definitions[key].append(str(rule))
#                     # Rules with one head Literal
#                     elif isinstance(head, ast.LiteralSymbolic):
#                         key = util.pred_to_str(head.atom)
#                         definitions[key].append(str(rule))
#                     # Rules with disjunction in head
#                     elif isinstance(head, ast.HeadDisjunction):
#                         for elem in head.elements:
#                             if isinstance(elem.literal.atom, ast.AtomSymbolic):
#                                 key = None
#                                 if isinstance(elem.literal.atom.symbol, ast.TermUnaryOperation):
#                                     key = util.pred_to_str(elem.literal.atom, uniaryOp=True)
#                                 else:
#                                     key = util.pred_to_str(elem.literal.atom)
#                                 definitions[key].append(str(rule))
#                     # Rules with aggregate in head
#                     elif isinstance(head, ast.HeadAggregate):
#                         for elem in head.elements:
#                             key = util.pred_to_str(elem.literal.atom)
#                             definitions[key].append(str(rule))
#                     else:
#                         print(f"Rule head is of type : {type(head)}")
#                         print(f"Rule: {rule}")
#                 except Exception as e:
#                     print(f"Error Splitting Program: {e}")
#                     print(f"Rule: {rule}")
#                     import traceback
#                     print(traceback.format_exc())
#         ast.parse_string(self.lib, program, process_rule)
#         self.constraints = list(constraints)
#         self.definitions = dict(definitions)
#         return self.constraints, self.definitions

#     def reConstructProgram(self, updateProgram=True, constraints=None, definitions=None):
#         if constraints is None:
#             constraints = self.constraints
#         if definitions is None:
#             definitions = self.definitions
#         new_program_rules = []
#         seen_rules = set()
#         for constraint in constraints:
#             if constraint not in seen_rules:
#                 new_program_rules.append(constraint)
#                 seen_rules.add(constraint)
#         for key, rules in definitions.items():
#             for rule in rules:
#                 if rule not in seen_rules:
#                     new_program_rules.append(rule)
#                     seen_rules.add(rule)
#         reconstructedProgram = "\n".join(new_program_rules)
#         if updateProgram:
#             self.asp_program = reconstructedProgram
#         return reconstructedProgram

#     def processPipelinetransformers(self):
#         from functional_to_casp.casp.transformers.preprocessing import (
#             AggregateHeadBodyConditionTransformer,
#             NegatedComparisonHeadToBodyTransformer,
#             ChoiceGuardTransformer,
#             NotAggregateConstraintTransformer
#         )
#         transformers = [
#             NegatedComparisonHeadToBodyTransformer(),
#             ChoiceGuardTransformer(),
#             NotAggregateConstraintTransformer(),
#             AggregateHeadBodyConditionTransformer(self.lib),
#         ]
#         initial_asts = []
#         for ast_node in self.asp_program_ast:
#             initial_asts.extend(ASTUtils.split_multiple_aggregate_elements(ast_node))
#         current_asts = initial_asts
#         for tr in transformers:
#             next_asts = []
#             for stmt in current_asts:
#                 out = tr.rewrite_rule(stmt)
#                 if isinstance(out, list):
#                     next_asts.extend(out)
#                 else:
#                     next_asts.append(out)
#             current_asts = next_asts
#         self.asp_program_ast = current_asts
#         self.update_program_string()
#         return self.asp_program
