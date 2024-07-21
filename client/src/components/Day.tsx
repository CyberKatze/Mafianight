import React, { useState } from "react";
import sun from "../assets/images/sun.svg";
import moon from "../assets/images/moon.svg";
import PlayerCard from "./PlayerCard";
import { useNavigate } from "react-router-dom";
import { players } from "../lib/mockData";

interface DayProps {
  handlePhaseChange: () => void;
}
const Day: React.FC<DayProps> = ({ handlePhaseChange }) => {
  const [number, setNumber] = useState(1);
  const navigate = useNavigate();

  return (
    <div className="bg-midnight space-y-4 flex flex-col p-4 items-center max-w-2xl mx-auto">
      <div className="flex flex-row justify-between w-full ">
        <div
          className="bg-peach flex justify-evenly font-bold w-1/6 py-2 text-2xl rounded-xl"
          onClick={() => setNumber(number + 1)}
        >
          <img src={sun} className="w-8 h-8" />
          <div className="text-2xl">{number}</div>
        </div>
        <button
          className="bg-stormy flex justify-evenly space-x-1 p-1 items-center font-bold py-2 text-2xl rounded-xl"
          onClick={handlePhaseChange}
        >
          <img src={moon} className=" w-8 h-8" />
          <p className="text-sm">Next Night</p>
        </button>
      </div>
      <div className="flex flex-row justify-between w-full">
        <div className="flex flex-col space-y-2">
          <h5 className="text-xl text-lavender text-left">Live Players </h5>

          {players
            .filter((player) => player.alive)
            .map((player) => (
              <div className="flex flex-row items-center space-x-2">
                <PlayerCard key={player.id} player={player} />
              </div>
            ))}
        </div>
        <div className="flex flex-col space-y-2">
          <h5 className="text-xl text-lavender text-right">Deads</h5>
          {players
            .filter((player) => !player.alive)
            .map((player) => (
              <PlayerCard key={player.name} player={player} />
            ))}
        </div>
      </div>
      <button onClick={() => navigate('/voting')} className="bg-lemon flex justify-evenly bg-lavendor space-x-1 px-6 p-3 items-center font-bold text-2xl rounded-xl">
        Voting
      </button>
    </div>
  );
};

export default Day;
