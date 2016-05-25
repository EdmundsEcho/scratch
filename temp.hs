-λ> let (Node a (t:ts)) = mytree; in a
4
λ> let (Node a (t:ts)) = mytree; in t
Node 3 []
λ> let (Node a (t:ts)) = mytree; in ts
[Node 5 [],Node 3 [],Node 1 [Node 4 [Node 0 []],Node 2 []]]
λ> :l ticktack_perm_v1
[1 of 1] Compiling Main             ( ticktack_perm_v1.hs, interpreted )
Ok, modules loaded: Main.
λ> let (Node a (t:ts)) = mytree; in a
4
λ> let (Node a (t:ts)) = mytree; in t
Node 3 [Node 1 [Node 2 []]]
λ> let (Node a (t:ts)) = mytree in ts
[Node 5 [],Node 3 [],Node 1 [Node 4 [Node 0 []],Node 2 []]]
λ> - Scratch file
