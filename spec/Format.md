# Entity Fields

## Data
- Unique ID
- Source ID
- Descriptive tags
- DateTime information?
- Checksum

## Blob
- Data ID
- Medium ID
- Origin?
- Offset within medium
- Length

## Transformation
- Input
    - Blobs
    - Direct Inputs
- Output Blobs
- Optional Function ID
- Optional Inverse Transformation
    - Input
        - Blobs
        - Direct Inputs
    - Output Blobs
    - Function ID

## Transformation Function
- Unique ID
- Code/Definition

## Medium
- Unique ID
- Location/Descriptive tags
- Size?

## Source
- Unique ID
- Descriptive tags

## Transformer
- Unique ID
- Descriptive tags

# DRT Layout

- Header

# DRT Types

Overall there are two types of DRT datastructures. One is the operational DRT log which sould be known as DRTL. And a recovery tree which should be known as DRTT.

DRTL is to be used for online logging of DRT data, it is essentially an append only log where transformations and data are recorded. This is done to minimise performance impact on the application generatign DRT online. It closely resembles a journal in a filesystem.

The second type DRTT is an index structure that can be created from DRTL, this structure is used to optimise query performance during recovery procedure. And additionally used to store DRT data in offline mode.
