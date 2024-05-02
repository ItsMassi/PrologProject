import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [painting, setPainting] = useState(false);
  const [rowClueSat, setRowsClueSat] = useState(null); // Initialize as an empty array
  const [colClueSat, setColClueSat] = useState(null);
  const [winningMessage, setWinningMessage] = useState('');


  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
        setRowsClueSat(new Array (response['RowClues'].length).fill(false));
        setColClueSat(new Array (response['ColumClues'].length).fill(false));

      }
    });
  }
 
  
function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const content = painting ? '#' : 'X'; // Content to put in the clicked square.
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        const newRowsClue=[...rowClueSat];
        const newColsClue=[...colClueSat];
        newRowsClue[i]=response['RowSat']=== 0? false: true;
        newColsClue[j]=response['ColSat'] === 0? false: true;
        setRowsClueSat(newRowsClue);
        setColClueSat(newColsClue);
        console.log(response);
        console.log(response['ResGrid']);

        const allRowsSatisfied = newRowsClue.every(clue => clue === true);
        const allColsSatisfied = newColsClue.every(clue => clue === true);

        if (allRowsSatisfied && allColsSatisfied) {
          setWinningMessage('You won!');
        }

      }
      setWaiting(false);
    });
  }

 
  if (!grid) {
    return null;
  }

  const statusText = 'Keep playing!';
  return (
    <div className="game">
       <Board
         grid={grid}
         rowsClues={rowsClues}
         colsClues={colsClues}
         rowClueSat={rowClueSat}
         colClueSat={colClueSat}
         onClick={(i, j) => handleClick(i, j)}
       />
       <div className="game-info">
         {statusText}
         {winningMessage && <div>{winningMessage}</div>}
         <div>
           <input type="checkbox" id="checkboxInput" value={painting} onChange={e => setPainting(e.target.checked)}/>
           <label for="checkboxInput" class="toggleSwitch"></label>
         </div>
       </div>
    </div>
   );

}
export default Game;