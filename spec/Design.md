### One DRT per system or multiple?

One DRT would imply that all transformers and the source(s) agree where to store the DRT information. This is very difficult to achieve in practive, and even further, because all transformers may not be known ahead of time not possible.

As such each data processign application, source or transformer will generate its own DRT. Then each following transformer will be responsible for taking in previous DRT (read only access) and generating its own DRT. Forming a sort of DRT chain. This way each transformer application can have a more optimal access pattern to the data.

Hence when a source generates a data item, it would log this data item to its own DRT data log. But then another problem appears, how dose the next transformer figure out which DRT item it is operating on? 

Well importantly transformers do not operate on DRT data items. They operate on blobs. Lets take an example: 

A user writes a file to an SMB server, SMB server records the file as the user data. And generates a blob for it. The blob's medium is /file.txt and offset 0. The underlying filesystem will then operate on this file. Possibly break it up into blocks and place them non contiguously. The transformation is split. Each blob has now a medium of /dev/sda and some offset unique to each blob.

Storing all transformations a filesystem does to all files is pointless and not feasible. The filesystem somehow needs to know which inputs into its world are related to DRT.

Same issue then continues for the storage controller. Perhaps the higher level DRT should contain a map of a medium? And this medium should be associated with the next transformer?

### How does a transformer know when it transforms a DRT element?

A transformer application (filesystem, storage controller, database) may tranform DRT data blobs as well as well as other data blobs that are not interesting to DRT or the use case. As such DRT has no business recording or operating on this data and it should be excluded. Since a transformer operates on blobs (and not data items) it needs to identify whether a piece of data it is performing a transformation on belongs to one of the DRT blobs. How does it do that?

Well the transformer has to be either told that an element it is processing is a DRT element, or it needs to ask. In case it is being told at the time the transformation is triggered it should have no problem determining this information. If it has to ask, either the upper layer or the DRT itself needs to be able to answer that question. To embed this functionality into DRT may not be generalisable and slow. As such being told is a better approach.

And when its told, it does not mean the core algorithm of the transformation needs to know, but some entity that is aware of the transformation Inputs and Outputs.

### A fully offline DRT

It may not be neccessary to generate DRT continuosly/online. Since performing massive data operations is a relatively uncommon task (due to its cost) it is likely that data shall remain in state recorded at some point in time. If such is the case, a DRT could be generated very efficiently in an optimised form. And then stored separately. In which case the most recent DRT would be used for recovery, and some data may have moved, or become missing, but the majority shall remain.

In such case it would be sufficient to ask the source what data it has. And then to ask each transformer as to how the data was changed. Theoretical DAG below could be constructed from this information during DRT generation. This is a nice and easy approach to the problem, and perhaps a good way to start.

### Extracting a data item?

Theoretically it is about traversing a DAG of blobs where each edge is a transformation. We start at the data node. Then locate the source blob. And the source blob would point to a transformation. The transformation would identify the output blobs. Then recurse until all leafs our found. From leafs traverse backwards through the tree performing inverse transformations to get back to source blob. At that point we have the original data.

But it is also about being able to update this weird data structure efficiently. Although that depends on the update frequency required. If the datastructure is updated on the fly/online performance will be terrible. If it is done offline on periodic basis it may be ok. However even in those cases the transformer may only know what the transformation is while it is performing it.

As such the transformations are probably best written to an append only journal online, and then update the main datastructure offline. SSTable like and garbage collection like approach.

While the approach described above would be quite efficient for recovery process, the duration of the recovery process itself is non critical, if it takes a day to recover all data, so be it. This is last line of defense, when everything else has gone bad. If it takes time, it is ok.
