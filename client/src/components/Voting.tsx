import React, { useState } from 'react';
import { PlayerWithRole } from '../lib/types';


type VotingComponentProps = {
  player: PlayerWithRole | null;
  players: PlayerWithRole[];
  onSelectionChange: (isChekced: boolean, playerId: string, selectedId: string) => void;
};

const VotingComponent = ({ player, players, onSelectionChange }: VotingComponentProps) => {
  const [checkedItems, setCheckedItems] = useState<string[]>([]);

  const handleCheckboxChange = (event: React.ChangeEvent<HTMLInputElement>, playerId: string, selectedId: string) => {
    const isChecked = event.target.checked;
    const item = `${playerId}-${selectedId}`;

    if (isChecked) {
      setCheckedItems([...checkedItems, item]);
    }
    else {
      console.log(checkedItems);
      setCheckedItems(checkedItems.filter(checkedItem => checkedItem !== item));
    }

    onSelectionChange(isChecked, playerId, selectedId); // Pass ID only when checked
  };

  return (
    <div>
      {player ? (
        <div
          key={player.name}
          className="relative max-w-sm rounded bg-gradient-to-t from-blue to-lavender overflow-hidden shadow-lg m-4"
        >
          <div className={` ${player.role.mafia ? 'bg-red' : 'bg-green'} absolute w-[9ch] text-center top-3 pt-14 pb-2 left-3 angle-label text-text-inverted px-2 py-1 font-semibold rounded`}>  
              {player.role?.mafia ? 'Mafia' : 'Citizen'}
    </div> 
          <img className="w-full " src={player.role.avatar} alt={player.name} />
          <div className="px-6 py-4">
            <p className="text-text-inverted text-2xl text-semibold">{player.name}
            </p>
            <div className='bg-base-default mt-4 p-4 rounded scrollbar-thumb-sky scrollbar-thumb-rounded  max-h-40 overflow-y-auto'>
              <ul className='list-none space-y-2'>
                {players.map((data, index) => (
                  player.id != data.id &&
                  (<li key={`${data.name}-${index}`} className='className="mt-2 cursor-pointe"'>
                    <input
                      type="checkbox"
                      className="form-checkbox h-5 w-5 text-text-default"
                      readOnly
                      checked={checkedItems.includes(`${player.name}-${data.name}`)}
                      onChange={(e) => handleCheckboxChange(e, player.name, data.name)}
                    />
                    <span className="text-text-default ml-2">{data.name}</span>
                  </li>)
                ))}
              </ul>
            </div>
          </div>
        </div>
      ) : (
        <p>No more data to display.</p>
      )}
    </div>
  );
};

export default VotingComponent;
