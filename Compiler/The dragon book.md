#compiler #reading-note

# 1 Concepts of Syntax Directed Methods

>[!definition] translation scheme
> - A _(Syntax-directed) translation scheme_ is a notation for attaching program fragments to the productions of a grammar.
> - Program fragments embedded within production bodies are called _semantic actions_^[this is just a way to represent translation scheme in the gramma itself]

## 1.1 SDD

>[!definition] syntax-directed definition
>A _syntax-directed definition_ associates
> 1. With each grammar symbol, a set of _attributes_, and
> 2. With each production, a set of _semantic rules_ for computing the values of the attributes associated with the symbols appearing in the production.
>
>A SDD that involves only synthesised attributes are called _S-attributed_, which can be implemented naturally in conjunction with an LR parser.
>A SDD without side effects is called an _attribute gramma_.

>[!definition] Attribute
>An attribute is any _quantity_ associated with a _programming construct_. 
>An attribute is said to be 
>- _synthesised_ if its value at a parse-tree node $N$ is determined from attribute values at the children of $N$ and at $N$ itself.
>- _inherited_ if its value is determined from $N$ itself, its parent and its siblings.
>```haskell
>type Synthesised pass1 pass2 = Term pass1 -> Term pass2
>type Inherited   pass1 pass2 = Maybe (Term pass1) 
>                             -- ^ parent node 
>                             -> Term pass1
>                             -- ^ current node
>                             -> Term pass2
>```
>Inherited attributes are useful when the structure of a parse tree does not "match" the AST.^[for detail see chapter 5.1]

>[!definition] S-attributed
> An SDD is _S-attributed_ if every attribute is synthesised.

>[!definition] L-attributed
>The idea behind this is that, between the attributes associated with a production body, dependency-graph edges can go from _left to right_, but not from right to left, hence "L-attributed".

>[!remark] 
>In a Haskell implementation using TTG^[trees that grow, for detail see the [GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance)]
>- _nonterminals_ are nodes of the AST, embodies as type constructors of terms;
>- _production rules_ are data constructors;
>- _attributes_ as embodies as _extension fields_;
>- _semantic rules_ are [[F-algebra]], `{haskell} TermF attr -> attr`
>- _semantic actions_ are of type `{haskell} Monad m => TermF attr -> m attr`
>- _translation schemes_ are function of type `{haskell} Term Pass1 -> Term Pass2`

>[!example] syntax-directed definition
>![[Pasted image 20241110000042.png]]

## 1.2 Symbol Table

>[!note] 
>The role of a symbol table is to pass information from declarations to using site.
> - _semantic actions_ can writes information to the _symbol table_;
> - _production rules_ that mentions a name reads from the table.
> f
>```haskell
>type SemanticAction m = TermF attr -> StateT SymTbl m attr
>```


When implemented in an imperative language:
```haskell
data SymTbl a = MkSymTbl 
  { _tblStore :: Map Name a      
  -- ^ single store of table entries
  , _tblMods  :: Stack [(Name, Maybe a)]
  {- ^ a stack of modifications to the store, 
       each entry for a nested scope -}
  }
```
when implemented in a functional language:
```haskell
data SymTbl a = MkSymTbl 
  { _tblCurr    :: Map Name a      
  -- ^ single store of table entries
  , _tblParents :: [Map Name a]
  -- ^ store of table entries for parent scopes
  }
```

# 2 Parsing techniques

There are 2 types of parsers differentiated by how parse trees are buildup,
1. top-down parser, build the tree from the root, usually seen in hand-written parsers;
2. bottom-up parser, build the usually used by parser generators;
3. universal parser, usually too inefficient to adopt in production code.

>[!definition] ambiguity
> A gramma is ambiguous if exists 2 different parse trees that yield the same token stream.

>[!definition] derivation
> Production rules can be think as rewrite rules in a rewrite system in which nonterminals are rewritten using production rules. 
> 1. In leftmost derivation, always choose the first nonterminal to expand;
> 2. In rightmost derivation, always choose the last nonterminal.

>[!definition] sentence
> A sentential form is a sequence of symbols that can be derived from the start symbol.
> A sentence of a grammar is a sentential form with no non-terminales.

>[!theorem] elimination of left recursion
> A grammar is _left recursive_ if it has the nonterminal $A$ such as there is a derivation $A \Rightarrow A \alpha$. 
> In general, we can eliminate it by transforming 
> $$ A \to A\alpha_1\; |\; \cdots \;|\; A\alpha_m \;|\; \beta_1\;|\;\cdots\;|\;\beta_n $$
> to 
> $$ A \to \beta_1A' \;|\; \cdots\;|\;\beta_nA' \; $$
> $$ A' \to \alpha_1A' \;|\;\cdots \;|\;\alpha_mA' \; |\; \varepsilon$$

>[!theorem] left factoring
> A _LL parser_ needs all production rules of every nonterminal have non-overlapping first set.
> $$ A \to \alpha\beta_1\;|\;\cdots\;|\;\alpha\beta_n\;|\;\gamma $$
> transforms to 
> $$ A \to \alpha A' \;|\;\gamma $$
> $$ A' \to \beta_1\;|\;\cdots\;|\;\beta_n $$

## 2.1 Top-down parser


>[!definition] LL(1) Parser
>The first "L" in LL(1) stands for scanning the input from left to right, the second "L" for producing a leftmost derivation, and the "1" for using one input symbol of lookahead at each step to make parsing action decisions.

For detail see [[Parsec]].

During topdown parsing, `FIRST` and `FOLLOW` allow us to choose which production to apply, based on the next input symbol.

```haskell
{-# LANGUAGE GADTs, TemplateHaskell #-}
import Data.Some

import Data.Map.Strict qualified as Map
import Lens.Micro ()
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Extras (view)

type IsTerm = Bool
type Terminal = GramSyn True
type NonTerminal = GramSyn False

data GramSyn (t :: IsTerm) where 
  Terminal :: Text -> GramSyn True
  Epsilon  ::         GramSyn True
  NonTerm  :: Text -> GramSyn False

deriving instance Eq (GramSyn is_term)

data Production a = MkProduction 
  { _prodHead :: NonTerminal 
  , _prodBody :: [Some GramSyn]
  , _prodExt  :: a
  }
  deriving (Show, Eq, Functor, Applicative)
  
$(makeLenses ''Production)

prodMatches :: Production a -> Production b -> Bool
prodMatches a b = _prodHead a == _prodHead b
                    && _prodBody a == _prodBody b
 
type Grammar a = Map.Map NonTerminal [Production a]

proper_grammar :: Grammar a -> Bool
proper_grammar = Map.mapWithKey check 
 where
  check sym prods = all ((== sym) . view prodHead) prods

getFirstSet :: Grammar () -> Grammar [Terminal]
getFirstSet MkProduction{..} = undefined
 where
  firstOf :: Some GramSyn -> [Terminal]
  firstOf (Some t@(Terminal _)) = pure t
  firstOf (Some t@(Epsilon))    = pure t
  firstOf (Some t@(NonTerm _))  = _
  
```

>[!theorem] FIRST
>To compute $FIRST(X)$ for all grammar symbols $X$, apply the following rules until no more terminals or $\varepsilon$ can be added to any $FIRST$ set.
> 1. If $X$ is a terminal, then $FIRST(X) = \{x\}$.
> 2. If $X\to\varepsilon$ is a production, then $\varepsilon\in FIRST(X)$.
> 3. If $X$ is a nonterminal and $X → Y_1\cdots Y_k$ is a production for some $k \ge 1$, then 
>    - place $a$ in $FIRST(X)$ if for some $i$, $a \in FIRST(Y_i)$, and $\varepsilon$ is in all of $FIRST(Y_1),\cdots , FIRST(Y_{i-1})$; that is, $Y_1 ...Y_{i-1} \Rightarrow \varepsilon$.
>    - If $\forall 1\le j\le k,\; \varepsilon\in FIRST(Y_j)$, then $\varepsilon \in FIRST(X)$.

>[!theorem] FOLLOW
>Let $S$ be the start symbol, and $\$$ denotes EOF.
>1. $$\$\in FOLLOW(S)$$
>2. If there is a production $A\to \alpha B \beta$ then $$FIRST(\beta) - \{\varepsilon\} \subset FOLLOW(B)$$
>3. If there is a production $A\to\alpha B$ then $$ FOLLOW(A)\subset FOLLOW(B)$$
>4. If there is a production $A\to\alpha B\beta$ and $\varepsilon\in FIRST(\beta)$, then $$ FOLLOW(A)\subset FOLLOW(B)$$

## 2.2 Error Recovery

Panic mode error recovery is based on the idea of skipping over symbols on the input until a token in a selected set of synchronising tokens appears.^[for detail see chapter 4.4.5]

## 2.3 Bottom-up parser

Bottom-up parsing during a left-to-right scan of the input constructs a rightmost derivation in reverse.

![[Pasted image 20241116133229.png]]

>[!definition] $LR(k)$
> 1. the "L" is for left-to-right scanning of the input, 
> 2. the "R" for constructing a _rightmost_ derivation in reverse, and 
> 3. the $k$ for the number of input symbols of _lookahead_ that are used in making parsing decisions.

For a grammar to be $LR(k)$ we must be able to recognise the occurrence of the right side of a production in a right-sentential form with $k$ input symbols of lookahead. This requirement is far less stringent than that for $LL(k)$ grammars where we must be able to recognise the use of a production seeing only the first $k$ symbols of what its right side derives.

### 2.3.1 SLR (Simple LR)

>[!definition] $LR(0)$ Item
>An $LR(0)$ item of a grammar $G$ is a production of $G$ with a dot at some position of the body.

```haskell
-- | DotPos states where the dots is, start from 0
type DotPos = Int
-- | state for shift reduce algorithm
type LR0_Item = Producton DotPos
```

- Item $A \to X\cdot Y Z$ indicates that we have just seen on the input a string derivable from $X$ and that we hope next to see a string derivable from $Y Z$. 
- Item $A \to XYZ\cdot$ indicates that we have seen the body $XYZ$ and that it may be time to reduce $XYZ$ to $A$.

```haskell
type LR0_Automaton = [LR0_ItemSet]
type LR0_ItemSet   = [LR0_Item]
type ProdSet a = [Production a]

-- the following functions compiles the NDFA to DFA

-- | the returned item set is a state in the DFA
closure :: ProdSet a -> LR0_ItemSet -> LR0_ItemSet
-- | this is the transition (shift action) in the DFA
goto :: LR0_ItemSet -> Some GrmSym -> LR0_ItemSet
```



# 3 IR

There are two most important intermediate representations:
1. Trees, such as parse trees and abstract syntax trees;
2. Linear representation, such as "three-address code".

Some information to collect:
1. type info
2. run type storage, include record width & field alignment

## 3.1 from AST to DAG

>[!definition] DAG
> A directed acyclic graph (here after called a DAG) for an expression identifies the _common subexpressions_ (subexpressions that occur more than once) of the expression.
>
> The difference is that a node $N$ in a DAG has more than one parent if $N$ represents a common subexpression;


```haskell
data ExprF r 
  = Val Double
  | Add r r
  | Sub r r
  | Mul r r
  | Div r r
 deriving (Show, Eq)

type Ast = Fix ExprF
type Dag = ExprF ValueNumber
type ValueNumber = Int  -- ^ reference to a Node
type DagNodeStore = HashMap Dag

instance Hashable Dag where
  hashWithSalt salt e = 
    -- the hash function at least recognise the 
    -- structure of node 
    undefined 

```

## 3.2 Tree-address code

```haskell
data Instr n = MkInstr 
  { op   :: !Op 
  , arg1 :: !(Addr n) 
  , arg2 :: !(Maybe (Addr n))
  , rst  :: !(Maybe (Addr n))
  }
 deriving (Show, Eq)
 
data Addr n 
  = Name  n          -- ^ varname / register 
  | Const !Double 
  | Label !Int       -- ^ symbolic label for goto
  | Temp  !Int       -- ^ compiler generated name
 deriving (Show, Eq)
```

>[!definition] SSA
>static single-assignment form requires a three address code to 
>1. all assignments are to variables with distinct names;
>2. the $\phi$-function is used to combine names defined in different control flow path.
>```haskell
> lookup (Phi x y) = lookup x <|> lookup y 
>```

