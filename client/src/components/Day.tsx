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
    <div className=" mt-4 bg-gradient-to-b from-mantle to-crust rounded space-y-4 max-h-90 flex flex-col p-4 items-center max-w-2xl mx-auto">
      <div className="flex flex-row justify-between w-full ">
        <button
          className="bg-peach hover:bg-yellow text-text-inverted flex justify-evenly items-center font-bold w-1/6 py-2 text-2xl rounded-xl"
          onClick={() => setNumber(number + 1)}
        >
          <img src={sun} className="w-8 h-8" />
          <div className="text-xl">{number}</div>
        </button>
        <button
          className="bg-stormy text-text-inverted items-center hover:bg-lavender flex justify-center space-x-1 p-1 items-end font-bold py-2 text-2xl rounded-xl"
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
              <div key={player.name} className="flex flex-row items-center space-x-2">
                <PlayerCard player={player} />
              </div>
            ))}
        </div>
        <div className="flex-col space-y-2 hidden sm:flex">
          <h5 className="text-xl text-lavender text-right">Deads</h5>
          {players
            .filter((player) => !player.alive)
            .map((player) => (
              <PlayerCard key={player.name} player={player} />
            ))}
        </div>
      </div>
      <button onClick={() => navigate('/voting')} className="text-text-inverted bg-gradient-to-l from-teal to-green hover:to-rosewater hover:from-rosewater flex justify-evenly  space-x-1 px-6 p-3 items-center font-bold text-xl rounded-xl">
        Voting
      </button>
    </div>
  );
};

export default Day;
