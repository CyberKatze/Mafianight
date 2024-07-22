import { PlayerWithRole } from '../lib/types';



const RoleCardComponent = ({ player }: { player: PlayerWithRole }) => {
  return (
    <div>
      {player ? (
        <div
          key={player.name}
          className="max-w-sm rounded bg-stormy border overflow-hidden shadow-lg m-4 transform transition duration-500 hover:scale-105 hover:shadow-2xl"
        >
          <img className="w-full" src={player.role?.avatar} alt={player.role?.name} />
          <div className="px-6 py-4">
            <p className="font-bold text-xl mb-2 text-red">{player.role?.name}</p>
            <p className="text-gray-700 text-base">{player.name}</p>
            <p
              className={`mt-2 font-semibold ${player.role?.mafia ? 'text-red' : 'text-green-500'
                }`}
            >
              {player.role?.mafia ? 'Mafia' : 'Not Mafia'}
            </p>
          </div>
        </div>
      ) : (
        <p>No more data to display.</p>
      )}
    </div>
  );
};

export default RoleCardComponent;
