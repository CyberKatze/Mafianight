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
          key={player.id}
          className="max-w-sm rounded bg-stormy border overflow-hidden shadow-lg m-4"
        >
          <img className="w-full " src={player.role.avatar} alt={player.name} />
          <div className="px-6 py-4">
            <p className="text-white text-2xl">{player.name}
              <span className={`mt-2 text-base font-semibold float-right ${player.role.mafia ? 'text-red' : 'text-green-500'
                }`}>
                {player.role.mafia ? ' Mafia' : ' Not Mafia'}
              </span>
            </p>
            <div className='bg-black mt-4 p-4 rounded max-h-40 overflow-y-auto'>
              <ul className='list-none space-y-2'>
                {players.map((data, index) => (
                  data.id != player.id &&
                  (<li key={`${data.id}-${index}`} className='className="mt-2 cursor-pointe"'>
                    <input
                      type="checkbox"
                      className="form-checkbox h-5 w-5 text-gray-600"
                      readOnly
                      checked={checkedItems.includes(`${player.id}-${data.id}`)}
                      onChange={(e) => handleCheckboxChange(e, player.id, data.id)}
                    />
                    <span className="text-gray-700 ml-2">{data.name}</span>
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
