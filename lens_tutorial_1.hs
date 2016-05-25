-- Contorl.Lens.Tutorial

-- atom.hs

{-# LANGUAGE TemplateHaskell #-}

import Control.Lens hiding (element)
import Control.Lens.Tutorial


shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)