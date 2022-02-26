module LRRep where
import Rep

type Tadd = STerm
type Tbinders = [([String], String)]
type Tbinders0 = Tbinders
type Tcall = STerm
type Tcase = (String, STerm)
type Tcases = [Tcase]
type Tcocase = STerm
type Tcomp = STerm
type Tconj = STerm
type Tconj2 = STerm
type Tdefbinder = ([(String, STerm)], String)
type Tdefbinders = [Tdefbinder]
type Tdisj = STerm
type Tdisj2 = STerm
type Tequals = STerm
type Texp = STerm
type Tfile = [Statement]
type Tlabbind = String
type Tlabbinds = [Tlabbind]
type Tlabterm = (String, STerm)
type Tlabterms = [Tlabterm]
type Tlabtype = (String, STerm)
type Tlabtypes = [Tlabtype]
type Tlambda = STerm
type Tlambdabinder = ([String], String)
type Tlambdabinders = [Tlambdabinder]
type Tlet = STerm
type Tmonad = STerm
type Tmul = STerm
type Tname = (String, String)
type Tnames = [(String, String)]
type Tnormal = STerm
type Tnot = STerm
type Tprefix = STerm
type Tprim = STerm
type Tstatement = Statement
type Tstruct = Tlabterms
type Tstructbind = Tlabbinds
type Tstructdef = [(String, STerm)]
type Tterm = STerm
type Tterms = [Tterm]

unary_call :: String -> STerm -> STerm
unary_call n a = SCall (SName n) [] a

binary_call :: String -> STerm -> STerm -> STerm
binary_call n a b = SCall (SCall (SName n) [] a) [] b

lambda_handler :: Tlambdabinders -> STerm -> STerm
lambda_handler [] x = x
lambda_handler ((lb,n):xs) x = SAbs lb n (lambda_handler xs x)

fresh_handler :: Tlambdabinders -> STerm -> STerm
fresh_handler [] x = x
fresh_handler ((lb,n):xs) x = unary_call "fresh" (SAbs lb n (fresh_handler xs x))

list_handler [] tail = tail
list_handler (x:xs) tail = binary_call "cons" x (list_handler xs tail)

reduction0 file = Just file
reduction1 stmt = [stmt]                    -- file: statement
reduction2 file stmt = (stmt:file)          --     | file statement
reduction3 name = Import name []            -- statement: "import" id
reduction4 name names = Import name names   --          | "import" id "using" names
reduction5 names = Export names             --          | "export" names
reduction6 name = [name]                    -- names: name
reduction7 name names = (name:names)        --      | name "," names
reduction8 id = (id, id)                    -- name: id
reduction9 from to = (from, to)             --     | id "⇒" id
--reduction10 = []                            -- indices:
--reduction11 index indices = (index:indices) -- indices: index indices
--reduction12 binders normal = ()           -- index: "(" binders ":" normal ")"
reduction13 id normal = TypeDecl id normal  -- statement: "def" id ":" normal
reduction14 id normal = Decl id normal      --          | "def" id "=" normal
reduction15 :: String -> Tlambdabinders -> Tnormal -> Tstatement
reduction15 id defbinders normal = Decl id (lambda_handler defbinders normal) --          | "def" id defbinders "=" normal
reduction16 term = term                     -- normal: term
reduction17 sd a b = SPi sd "_" a b         --       | structdef term "→" normal
reduction18 a b = SPi [] "_" a b            --       | term "→" normal
reduction19 :: Tstructdef -> String -> Tnormal -> Tnormal -> Tnormal
reduction19 sd i a b = SPi sd i a b         --       | structdef "(" id ":" normal ")" "→" normal
reduction20 :: String -> Tnormal -> Tnormal -> Tnormal
reduction20 i a b = SPi [] i a b            --       | "(" id ":" normal ")" "→" normal
reduction21 x y = SEq SHole x y             --       | term "≡" term
reduction22 a x y = SEq a x y               --       | "⟦" normal "⟧" term "≡" term 
reduction23 x y = SAnn x y                  --       | term ":" normal
reduction24 x = unary_call "_neg_" x        --       | "-" normal
reduction25 t = [t]                         -- terms: term
reduction26 t ts = (t:ts)                   --      | term "," terms
reduction27 t = t                           -- term: lambda
reduction28 t = t                           -- lambda: monad
reduction29 lb t = lambda_handler lb t      --       | "λ" lambdabinders "." lambda
reduction30 lb t = fresh_handler lb t       --       | "∃" binders "." lambda
reduction31 t = t                           -- monad: let
reduction32 a b = binary_call "_bind_" a (SAbs [] "_" b) --      | let ";" monad
reduction33 n a b = binary_call "_bind_" a (SAbs [] n b) --      | id "←" cocase ";" monad
reduction34 t = t                           -- let: cocase
reduction35 n a b = SBind n a b             --      | id "is" cocase ";" let
reduction36 t = t                           -- cocase: disj
reduction37 n t = SAbs [] n t               --       | id "↦" cocase
reduction38 c = SCocase c                   --       | "⟮" cases "⟯"
reduction39 t = t                           -- disj: conj2
reduction40 = binary_call "_lb_"            --     | conj2 "⊔" disj
reduction41 t = t                           -- conj2: disj2
reduction42 = binary_call "_hb_"            --     | disj2 "⊓" conj2
reduction43 t = t                           -- disj2: conj
reduction44 = binary_call "_or_"            --      | conj "∨" disj2
reduction45 t = t                           -- conj: not
reduction46 = binary_call "_and_"           --     | not "∧" conj
reduction47 t = t                           -- not: equals
reduction48 = unary_call "_not_"            --    | "¬" not
reduction49 t = t                           -- equals: comp
reduction50 = binary_call "_lt_"            --       | comp "<" comp
reduction51 = binary_call "_gt_"            --       | comp ">" comp
reduction52 = binary_call "_le_"            --       | comp "≤" comp
reduction53 = binary_call "_ge_"            --       | comp "≥" comp
reduction54 = binary_call "_eq_"            --       | comp "=" comp
reduction55 = binary_call "_ne_"            --       | comp "≠" comp
reduction56 t = t                           -- comp: add
reduction57 = binary_call "_comp_"          --     | add "∘" comp
reduction58 t = t                           -- add: mul
reduction59 = binary_call "_add_"           --    | mul "+" add
reduction60 = binary_call "_sub_"           --    | mul "-" add
reduction61 t = t                           -- mul: prefix
reduction62 = binary_call "_mul_"           --    | prefix "*" mul
reduction63 = binary_call "_div_"           --    | prefix "/" mul
reduction64 t = t                           -- prefix: exp
reduction65 = unary_call "_inv_"            --       | "~" prefix
reduction66 t = t                           -- exp: call
reduction67 = binary_call "_exp_"           --    | call "^" exp
reduction68 t = t                           -- call: prim
reduction69 t s a = SCall t s a             --     | call struct prim
reduction70 t c = SCase t SHole c           --     | call "⟮" cases "⟯"
reduction71 t mot c = SCase t mot c         --     | call "⟦" normal "⟧" "⟮" cases "⟯"
reduction72 i = SName i                     -- prim: id
reduction73 t = t                           --     | "(" normal ")"
reduction74 = SStar                         --     | "⋆"
reduction75 = SIntegerConst                 --     | integer
reduction76 = SRealConst                    --     | real
reduction77 = SStringConst                  --     | string
reduction78 = SLRecord [] Nothing           --     | "{" "}"
reduction79 = SORecord                      --     | "{" terms "}"
reduction80 a = SLRecord a Nothing          --     | "{" labterms "}"
reduction81 a b = SLRecord a (Just b)       --     | "{" labterms "|" normal "}"
reduction82 = SName "nil"                   --     | "[" "]"
reduction83 a = list_handler a (SName "nil") --     | "[" terms "]"
reduction84 a b = list_handler a b           --     | "[" terms "|" term "]"
reduction85 = []                            -- struct:
reduction86 = []                            -- struct: "⟨" "⟩"
reduction87 a = a                           --       | "⟨" labterms "⟩"
reduction88 a = error "not supported"       --       | "⟨" terms "⟩"
reduction89 = []                            -- structdef: "⟨" "⟩"
reduction90 a = a                           --          | "⟨" labtypes "⟩"
reduction91 = []                            -- structbind:
reduction92 = []                            -- structbind: "⟨" "⟩"
reduction93 a = a                           --           | "⟨" labbinds "⟩"
reduction94 x = [x]                         -- labterms: labterm
reduction95 x xs = (x:xs)                   -- labterms: labterm "," labterms
reduction96 a b = (a, b)                    -- labterm: id "=" normal
reduction97 a = [a]                         -- labtypes: labtype
reduction98 x xs = (x:xs)                   -- labtypes: labtype "," labtypes
reduction99 a b = (a, b)                    -- labtype: id ":" normal
reduction100 x = [x]                        -- labbinds: labbind
reduction101 x xs = (x:xs)                  --         | labbind "," labbinds
reduction102 x = x                          -- labbind: id
reduction103 x = [x]                        -- cases: case
reduction104 x xs = (x:xs)                  --      | case "," cases
reduction105 i db t = (i, lambda_handler db t) -- case: id binders0 "⇒" normal
reduction106 = []                           -- binders0:
reduction107 b = b                          -- binders0: binders
reduction108 x = [([],x)]                   -- binders: id
reduction109 x xs = (([],x):xs)             --        | id binders
reduction110 x = [([],x)]                   --        | "◊" id
reduction111 x xs = (([],x):xs)             --        | "◊" id binders
reduction112 x = [x]                        -- defbinders: defbinder
reduction113 x xs = (x:xs)                  --           | defbinder defbinders
reduction114 i = ([], i)                   -- defbinder: id
reduction115 :: Tstructdef -> String -> Tdefbinder
reduction115 sd i = (sd, i)                -- defbinder: structdef id
reduction116 x = [x]                        -- lambdabinders: lambdabinder
reduction117 x xs = x:xs                    --              | lambdabinder lambdabinders
reduction118 sb id = (sb, id)               -- lambdabinder: structbind id
