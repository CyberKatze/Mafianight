import { PlayerWithRole } from '../lib/types';



const RoleCardComponent = ({ player }: { player: PlayerWithRole }) => {
  return (
    <div>
      {player ? (
        <div
          key={player.name}
          className="max-w-sm rounded bg-gradient-to-t from-blue to-lavender border overflow-hidden shadow-lg m-5 transform transition duration-500 hover:scale-105 hover:shadow-2xl"
        >
          <div className={` ${player.role.mafia ? 'bg-red' : 'bg-green'} absolute w-[9ch] text-center top-3 pt-14 pb-2 left-3 angle-label text-text-inverted px-2 py-1 font-semibold rounded`}>  
              {player.role?.mafia ? 'Mafia' : 'Citizen'}
    </div> 
          <img className="w-full" src={player.role?.avatar} alt={player.role?.name} />
          <div className="px-6 py-4">
            <p className="font-bold text-xl mb-2 text-text-inverted">{player.role?.name}</p>
            <p className="text-mantle">{player.name}</p>
          </div>
        </div>
      ) : (
        <p>No more data to display.</p>
      )}
    </div>
  );
};

export default RoleCardComponent;
