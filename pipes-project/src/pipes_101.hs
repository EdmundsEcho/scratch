import Pipes
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import Pipes.Safe

main =  runSafeT $ runEffect $ Text.readFile "data.txt" 
        >-> Text.writeFile "outFile.txt"