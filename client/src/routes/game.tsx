import React, { useState } from "react";
import Day from "../components/Day";
import Night from "../components/Night";
// import { useParams } from "react-router-dom";
// import { gameAtom } from "../lib/store";
// import { useAtom } from "jotai";

const Game: React.FC = () => {
  // const { gameId } = useParams<{ gameId: string }>();
  // const [game, _setGame] = useAtom(gameAtom);
  // console.log(game);
  const [day, setDay] = useState(true);
  return (
    <>

      {day ? <Day handlePhaseChange={() => setDay(false)} /> : <Night handlePhaseChange={() => setDay(true)} />}

    </>
  );
};

export default Game;
