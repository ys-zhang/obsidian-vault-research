# Double Connected Edge List

A **double connected edge list** keeps a record for each, **face**, **edge** and **vertex**
-   edge points to its neighbors.
-   edge points to the two faces which are bounded by the edge.

![[Double-Edge-linked-list.png]]
>A **graph** is **planar** if it can be drawn in a plane without graph edges crossing (i.e., it has graph crossing number 0). 
>A **planar embedding** is an embedding of a graph drawn in the plane such that *edges intersect only at their endpoints*.

## Basic Operation:

-  walk around the boundary of a given face
- access one face from an adjacent one if we are given a common edge
- visit all the edges around a given vertex

```go
type Vertex struct {
	Coordinate
 	IncidentEdge *HalfEdge // an arbitrary edge that origin from it
}

type Face struct {
	OuterComponent *HalfEdge      // some half-edge of the outer boundary of the face
	InnerComponents []*HalfEdge   // each half page stands for a hole in the face
  	AttributeData   interface{}
} 

type HalfEdge struct {
	Twin *HalfEdge
	IncidentFace *Face
	Origin *Vertex
	Next *HalfEdge
	Prev *HalfEdge
}

func (e *HalfEdge) Destination() *Vertex {
	return e.Twin.Origin
}

func (v *Vertex) AdjacentVerteces() []*Vertex {
	edge := v.IncidentEdge
 	rst = make([]*Vertex, 1)
 	rst[0] = edge.Destination()
 	e := edge.Prev.Twin
 	for e != edge {
		rst = append(rst, e.Destination())
		e = e.Prev.Twin
	}
	return 
}
```
