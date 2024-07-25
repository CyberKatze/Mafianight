import React from "react";
import type { PlayerWithRole } from "../lib/types";

interface Props {
  player: PlayerWithRole;
  active?: boolean;
}
const PlayerCard: React.FC<Props> = ({ player, active = true }) => {
  return (
    <button className={
      active
        ? "flex transform trnasition bg-surface-dark hover:-translate-y-1 hover:bg-mauve text-text-inverted shadow p-2 rounded-lg justify-between py-2"
        : "flex transform transition bg-surface-light shadow p-2 rounded-lg justify-between py-2 cursor-not-allowed"}>
      <div className="flex items-center gap-2 ">
        <img
          src={player.role.avatar}
          className="object-cover  color-red w-12 h-12 rounded-md shrink-0 dark:border-none"
          loading="lazy"
          alt="nejaa badr"
        />
        <div>
          <p
            className={`text-sm  ${player.role.mafia ? "text-red" : "text-mint"
              }`}
          >
            {player.role.name}
          </p>
          <p className="text-text-default dark:text-gray-300 hover:text-primary-800">
            {player.name}
          </p>
        </div>
      </div>
    </button>
  );
};

export default PlayerCard;
