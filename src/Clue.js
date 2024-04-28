import React from 'react';

function Clue({ clue ,satClue}) {
    return (
        <div className={"clue"} >
            {clue.map((num, i) =>
                <div className={satClue? "satClue": "unsatClue"} key={i}>
                    {num}
                </div>
            )}
        </div>
    );
}



export default Clue;