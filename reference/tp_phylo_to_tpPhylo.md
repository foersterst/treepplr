# Convert an `ape::phylo` tree into a TreePPL tree

Builds the nested list that TreePPL reads as a binary tree:

## Usage

``` r
tp_phylo_to_tpPhylo(
  phy,
  leaf_ctor = "Leaf",
  node_ctor = "Node",
  leaf_extra = function(i) list(),
  node_extra = function(i) list()
)
```

## Arguments

- phy:

  A rooted, binary tree of class `phylo` with branch lengths.

- leaf_ctor:

  Constructor name used for tips, e.g. `"Leaf"` or `"QTLeaf"`.

- node_ctor:

  Constructor name used for internal nodes, e.g. `"Node"` or `"QTNode"`.

- leaf_extra:

  A function of one tip number returning a named list of extra leaf
  fields.

- node_extra:

  A function of one internal node number returning a named list of extra
  node fields.

## Value

A nested list of class `tpPhylo`, ready for
[`tp_data()`](http://treeppl.org/treepplr/reference/tp_data.md) and
[`tp_write_tppl_json()`](http://treeppl.org/treepplr/reference/tp_write_tppl_json.md).
The tip labels of `phy` are kept in the attribute `"tip.label"`, in
ape's tip order; they are not written to JSON. See
[`print.tpPhylo()`](http://treeppl.org/treepplr/reference/print.tpPhylo.md)
for the console output.

## Details

    {"__constructor__": "Node", "__data__": {"left": .., "right": .., "age": 4}}
    {"__constructor__": "Leaf", "__data__": {"age": 0}}

Ages are times before the present: tips of an ultrametric tree get age 0
and the root gets the tree height. A node's `left` and `right` children
are taken in the order they appear in `phy$edge`.

Extra fields, such as a leaf `index` or a node `label`, are added with
`leaf_extra` and `node_extra`. Each receives the ape node number (tips
are `1:ape::Ntip(phy)`, the root is `ape::Ntip(phy) + 1`) and must
return a named list.

## Examples

``` r
phy <- ape::read.tree(text = "((A:4,B:4):6,(C:7,D:7):3);")

# ClockTree (diversification models)
tree <- tp_phylo_to_tpPhylo(phy)
tree
#> Phylogenetic tree of class <tpPhylo>, with 4 tips and 3 internal nodes.
#> 
#> Rooted; includes branch lengths; root age = 10

# TreeLabeled (host-repertoire models): a label on every leaf and node
tree_labeled <- tp_phylo_to_tpPhylo(
  phy,
  leaf_extra = function(i) list(label = i),
  node_extra = function(i) list(label = i)
)
```
