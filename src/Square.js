import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className={value==='#' ? "square painted": "square"} onClick={onClick}>
         {value==='X' ? value :null} 
        </button>
    );
}

export default Square;