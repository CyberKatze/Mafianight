import { RoleCard } from '../routes/rolecards';



const RoleCardComponent = ({ data }: { data: RoleCard }) => {
    return (
        <div>
            {data ? (
                <div
                    key={data.name}
                    className="max-w-sm rounded border overflow-hidden shadow-lg m-4 transform transition duration-500 hover:scale-105 hover:shadow-2xl"
                >
                    <img className="w-full" src={data.avatar} alt={data.name} />
                    <div className="px-6 py-4">
                        <p className="font-bold text-xl mb-2 text-red">{data.name}</p>
                        <p className="text-gray-700 text-base">{data.player}</p>
                        <p
                            className={`mt-2 font-semibold ${data.mafia ? 'text-red' : 'text-green-500'
                                }`}
                        >
                            {data.mafia ? 'Mafia' : 'Not Mafia'}
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