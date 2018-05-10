### One DRT is not feasible, it should a DRT per Transformer

One DRT would imply that all transformers and the source(s) agree where to store the DRT information. This is very difficult to achieve in practive, and even further, because all transformers may not be known ahead of time not possible.

As such each data processign application, source or transformer will generate its own DRT. Then each following transformer will be responsible for taking in previous DRT (read only access) and generating its own DRT. Forming a sort of DRT chain.



### How are blob and transformation related?

### How to locate all blobs of data efficiently?

### Extracting a data item?

Theoretically it is about traversing a DAG of blobs where each edge is a transformation. We start at the data node. Then locate the source blob. And the source blob would point to a transformation. The transformation would identify the output blobs. Then recurse until all leafs our found. From leafs traverse backwards through the tree performing inverse transformations to get back to source blob. At that point we have the original data.

But it is also about being able to update this weird data structure efficiently. Although that depends on the update frequency required. If the datastructure is updated on the fly/online performance will be terrible. If it is done offline on periodic basis it may be ok. However even in those cases the transformer may only know what the transformation is while it is performing it.

As such the transformations are probably best written to an append only journal online, and then update the main datastructure offline. SSTable like and garbage collection like approach.

While the approach described above would be quite efficient for recovery process, the duration of the recovery process itself is non critical, if it takes a day to recover all data, so be it. This is last line of defense, when everything else has gone bad. If it takes time, it is ok.
