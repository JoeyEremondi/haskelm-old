module Language.Elm.TH.BaseDecs where

import SourceSyntax.Location
import SourceSyntax.Expression
import SourceSyntax.Declaration
import SourceSyntax.Literal
import SourceSyntax.Pattern
import SourceSyntax.Location


baseDecs = [SourceSyntax.Declaration.Definition
          (SourceSyntax.Expression.Definition
             (SourceSyntax.Pattern.PVar "getType")
             (SourceSyntax.Location.L
                (SourceSyntax.Location.Span
                   (SourceSyntax.Location.Pos (1) (1))
                   (SourceSyntax.Location.Pos (1) (1)) (""))
                (SourceSyntax.Expression.Lambda
                   (SourceSyntax.Pattern.PData
                      "Object" [SourceSyntax.Pattern.PVar "d"])
                   (SourceSyntax.Location.L
                      (SourceSyntax.Location.Span
                         (SourceSyntax.Location.Pos (1) (1))
                         (SourceSyntax.Location.Pos (1) (1)) (""))
                      (SourceSyntax.Expression.Case
                         (SourceSyntax.Location.L
                            (SourceSyntax.Location.NoSpan "")
                            (SourceSyntax.Expression.App
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.NoSpan "")
                                  (SourceSyntax.Expression.App
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Var "Dict.lookup"))
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Literal
                                           (SourceSyntax.Literal.Str "__type")))))
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.Span
                                     (SourceSyntax.Location.Pos (1) (1))
                                     (SourceSyntax.Location.Pos (1) (1))
                                     (""))
                                  (SourceSyntax.Expression.Var "d"))))
                         [(SourceSyntax.Pattern.PData
                             "Just"
                             [SourceSyntax.Pattern.PData
                                "Json.String"
                                [SourceSyntax.Pattern.PVar "t"]],SourceSyntax.Location.L
                                                                   (SourceSyntax.Location.Span
                                                                      (SourceSyntax.Location.Pos
                                                                         (1) (1))
                                                                      (SourceSyntax.Location.Pos
                                                                         (1) (1))
                                                                      (""))
                                                                   (SourceSyntax.Expression.Var
                                                                      "t"))]))))
             Nothing),
        SourceSyntax.Declaration.Definition
          (SourceSyntax.Expression.Definition
             (SourceSyntax.Pattern.PVar "getCtor")
             (SourceSyntax.Location.L
                (SourceSyntax.Location.Span
                   (SourceSyntax.Location.Pos (1) (1))
                   (SourceSyntax.Location.Pos (1) (1)) (""))
                (SourceSyntax.Expression.Lambda
                   (SourceSyntax.Pattern.PData
                      "Object" [SourceSyntax.Pattern.PVar "d"])
                   (SourceSyntax.Location.L
                      (SourceSyntax.Location.Span
                         (SourceSyntax.Location.Pos (1) (1))
                         (SourceSyntax.Location.Pos (1) (1)) (""))
                      (SourceSyntax.Expression.Case
                         (SourceSyntax.Location.L
                            (SourceSyntax.Location.NoSpan "")
                            (SourceSyntax.Expression.App
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.NoSpan "")
                                  (SourceSyntax.Expression.App
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Var "Dict.lookup"))
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Literal
                                           (SourceSyntax.Literal.Str "__ctor")))))
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.Span
                                     (SourceSyntax.Location.Pos (1) (1))
                                     (SourceSyntax.Location.Pos (1) (1))
                                     (""))
                                  (SourceSyntax.Expression.Var "d"))))
                         [(SourceSyntax.Pattern.PData
                             "Just"
                             [SourceSyntax.Pattern.PData
                                "Json.String"
                                [SourceSyntax.Pattern.PVar "c"]],SourceSyntax.Location.L
                                                                   (SourceSyntax.Location.Span
                                                                      (SourceSyntax.Location.Pos
                                                                         (1) (1))
                                                                      (SourceSyntax.Location.Pos
                                                                         (1) (1))
                                                                      (""))
                                                                   (SourceSyntax.Expression.Var
                                                                      "c"))]))))
             Nothing),
        SourceSyntax.Declaration.Definition
          (SourceSyntax.Expression.Definition
             (SourceSyntax.Pattern.PVar "varNamed")
             (SourceSyntax.Location.L
                (SourceSyntax.Location.Span
                   (SourceSyntax.Location.Pos (1) (1))
                   (SourceSyntax.Location.Pos (1) (1)) (""))
                (SourceSyntax.Expression.Lambda
                   (SourceSyntax.Pattern.PData
                      "Object" [SourceSyntax.Pattern.PVar "d"])
                   (SourceSyntax.Location.L
                      (SourceSyntax.Location.Span
                         (SourceSyntax.Location.Pos (1) (1))
                         (SourceSyntax.Location.Pos (1) (1)) (""))
                      (SourceSyntax.Expression.Lambda
                         (SourceSyntax.Pattern.PVar "n")
                         (SourceSyntax.Location.L
                            (SourceSyntax.Location.Span
                               (SourceSyntax.Location.Pos (1) (1))
                               (SourceSyntax.Location.Pos (1) (1)) (""))
                            (SourceSyntax.Expression.Case
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.NoSpan "")
                                  (SourceSyntax.Expression.App
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.NoSpan "")
                                        (SourceSyntax.Expression.App
                                           (SourceSyntax.Location.L
                                              (SourceSyntax.Location.Span
                                                 (SourceSyntax.Location.Pos (1) (1))
                                                 (SourceSyntax.Location.Pos (1) (1))
                                                 (""))
                                              (SourceSyntax.Expression.Var "Dict.lookup"))
                                           (SourceSyntax.Location.L
                                              (SourceSyntax.Location.NoSpan "")
                                              (SourceSyntax.Expression.App
                                                 (SourceSyntax.Location.L
                                                    (SourceSyntax.Location.Span
                                                       (SourceSyntax.Location.Pos
                                                          (1) (1))
                                                       (SourceSyntax.Location.Pos
                                                          (1) (1))
                                                       (""))
                                                    (SourceSyntax.Expression.Var "show"))
                                                 (SourceSyntax.Location.L
                                                    (SourceSyntax.Location.Span
                                                       (SourceSyntax.Location.Pos
                                                          (1) (1))
                                                       (SourceSyntax.Location.Pos
                                                          (1) (1))
                                                       (""))
                                                    (SourceSyntax.Expression.Var "n"))))))
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Var "d"))))
                               [(SourceSyntax.Pattern.PData
                                   "Just" [SourceSyntax.Pattern.PVar "val"],SourceSyntax.Location.L
                                                                              (SourceSyntax.Location.Span
                                                                                 (SourceSyntax.Location.Pos
                                                                                    (1)
                                                                                    (1))
                                                                                 (SourceSyntax.Location.Pos
                                                                                    (1)
                                                                                    (1))
                                                                                 (""))
                                                                              (SourceSyntax.Expression.Var
                                                                                 "val"))]))))))
             Nothing),
        SourceSyntax.Declaration.Definition
          (SourceSyntax.Expression.Definition
             (SourceSyntax.Pattern.PVar "mapJson")
             (SourceSyntax.Location.L
                (SourceSyntax.Location.NoSpan "")
                (SourceSyntax.Expression.Lambda
                   (SourceSyntax.Pattern.PVar "f")
                   (SourceSyntax.Location.L
                      (SourceSyntax.Location.NoSpan "")
                      (SourceSyntax.Expression.Lambda
                         (SourceSyntax.Pattern.PData
                            "Array" [SourceSyntax.Pattern.PVar "l"])
                         (SourceSyntax.Location.L
                            (SourceSyntax.Location.NoSpan "")
                            (SourceSyntax.Expression.App
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.NoSpan "")
                                  (SourceSyntax.Expression.App
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Var "map"))
                                     (SourceSyntax.Location.L
                                        (SourceSyntax.Location.Span
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (SourceSyntax.Location.Pos (1) (1))
                                           (""))
                                        (SourceSyntax.Expression.Var "f"))))
                               (SourceSyntax.Location.L
                                  (SourceSyntax.Location.Span
                                     (SourceSyntax.Location.Pos (1) (1))
                                     (SourceSyntax.Location.Pos (1) (1))
                                     (""))
                                  (SourceSyntax.Expression.Var "l"))))))))
             Nothing),
        SourceSyntax.Declaration.Definition
          (SourceSyntax.Expression.Definition
             (SourceSyntax.Pattern.PVar "makeList")
             (SourceSyntax.Location.L
                (SourceSyntax.Location.Span
                   (SourceSyntax.Location.Pos (1) (1))
                   (SourceSyntax.Location.Pos (1) (1))
                   (""))
                (SourceSyntax.Expression.Lambda
                   (SourceSyntax.Pattern.PData
                      "Array" [SourceSyntax.Pattern.PVar "l"])
                   (SourceSyntax.Location.L
                      (SourceSyntax.Location.Span
                         (SourceSyntax.Location.Pos (1) (1))
                         (SourceSyntax.Location.Pos (1) (1))
                         (""))
                      (SourceSyntax.Expression.Var "l"))))
             Nothing),
        SourceSyntax.Declaration.Definition
          (SourceSyntax.Expression.Definition
             (SourceSyntax.Pattern.PVar "error")
             (SourceSyntax.Location.L
                (SourceSyntax.Location.Span
                   (SourceSyntax.Location.Pos (1) (1))
                   (SourceSyntax.Location.Pos (1) (1))
                   (""))
                (SourceSyntax.Expression.Lambda
                   (SourceSyntax.Pattern.PVar "s")
                   (SourceSyntax.Location.L
                      (SourceSyntax.Location.Span
                         (SourceSyntax.Location.Pos (1) (1))
                         (SourceSyntax.Location.Pos (1) (1))
                         (""))
                      (SourceSyntax.Expression.Case
                         (SourceSyntax.Location.L
                            (SourceSyntax.Location.Span
                               (SourceSyntax.Location.Pos (1) (1))
                               (SourceSyntax.Location.Pos (1) (1))
                               (""))
                            (SourceSyntax.Expression.Literal
                               (SourceSyntax.Literal.Boolean True)))
                         [(SourceSyntax.Pattern.PLiteral
                             (SourceSyntax.Literal.Boolean False),SourceSyntax.Location.L
                                                                    (SourceSyntax.Location.Span
                                                                       (SourceSyntax.Location.Pos
                                                                          (1) (1))
                                                                       (SourceSyntax.Location.Pos
                                                                          (1) (1))
                                                                       (""))
                                                                    (SourceSyntax.Expression.Var
                                                                       "s"))]))))
             Nothing)]