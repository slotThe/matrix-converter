# matrix

Small program to generate matrices and copy them to one of several
[predefined formats](#currently-supported-formats).

By default, finished entries are copied to the system clipboard and spat
out to stdout.

For example:

![](https://user-images.githubusercontent.com/50166980/156989797-07046d20-9b87-44b7-a05f-b1ab8ddd3c12.gif)

## Currently Supported Formats

- Clojure arrays: `[[1 2] [3 4]]`
- Haskell lists: `[[1,2],[3,4]]`
- LaTeX's `pmatrix`:
  ``` tex
  \begin{pmatrix}
    1 & 2 \\
    3 & 4 \\
  \end{pmatrix}
  ```
- Python arrays: `Matrix([[1,2],[3,4]])`
