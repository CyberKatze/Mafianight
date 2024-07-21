import React, { useState } from "react";
import moon from "../assets/images/moon.svg";
import sun from "../assets/images/sun.svg";
import type { Event } from "../lib/types";
import PlayerCard from "./PlayerCard";
import { players } from "../lib/mockData"

const events: Event[] = [
  { actionType: "kill", subject: "player 1", target: "player 6" },
];
interface NightProps {
  handlePhaseChange: () => void;
}
const Night: React.FC<NightProps> = ({ handlePhaseChange }) => {
  const [number, setNumber] = useState(1);
  return (
    <div className="bg-midnight  space-y-4 flex flex-col p-4 items-center max-w-2xl mx-auto">
      <div className="flex flex-row justify-between w-full ">
        <div
          className="bg-stormy flex justify-evenly font-bold w-1/6 py-2 text-2xl rounded-xl"
          onClick={() => setNumber(number + 1)}
        >
          <img src={moon} className=" w-8 h-8" />
          <div className="text-2xl">{number}</div>
        </div>
        <button
          className="bg-peach flex justify-evenly space-x-1 p-1 items-center font-bold py-2 text-2xl rounded-xl"
          onClick={handlePhaseChange}
        >
          <img src={sun} className=" w-8 h-8" />
          <p className="text-sm">Next Day</p>
        </button>
      </div>
      <div className="flex flex-row justify-between w-full">
        <div className="flex flex-col space-y-2">
          <h5 className="text-xl text-lavender text-left">Player with Action </h5>

          {players
            .filter((player) => player.role.name != "Villager")
            .map((player, index) => (
              <div className="flex flex-row items-center space-x-2">
                <PlayerCard active={index >= events.length} key={player.id} player={player} />
                {events
                  .filter((e) => e.subject === player.name)
                  .map((event) => (
                    <div className="flex flex-row space-x-1">
                      <p className="text-sm rounded-lg bg-pink py-1 px-2  bg- text-black ">{event.actionType}</p>
                      <p className="text-sm p-1 text-white ">{event.target}</p>
                    </div>
                  ))}
              </div>
            ))}
        </div>
        <div className="flex flex-col space-y-2">
          <h5 className="text-xl text-lavender text-right">Targeted Player</h5>
          {players.map((player) => (
            <PlayerCard key={player.id} player={player} />
          ))}
        </div>
      </div>
    </div>
  );
};

export default Night;
