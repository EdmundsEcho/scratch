{-# LANGUAGE TemplateHaskell #-}

import Control.Lens hiding (element)
import Control.Lens.Tutorial

--makeLenses ''Atom
--makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)

--makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)