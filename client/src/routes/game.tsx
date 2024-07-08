import React, { useState } from "react";
import Day from "../components/Day";
import Night from "../components/Night";

const Game: React.FC = () => {
  const [day, setDay] = useState(true);
  return (
    <>
    
  {day ? <Day handlePhaseChange={()=> setDay(false)} /> : <Night handlePhaseChange={()=> setDay (true)} />}
  
  </>
  );
};

export default Game;
