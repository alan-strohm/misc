package parse

type Pos int

type Node interface {
	Pos() Pos // position of the first character belonging to the node
	End() Pos // position of the first character immediately after the node.
}
