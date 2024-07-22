import React, { useEffect, useState } from 'react';
import { fetchRoles } from '../lib/rest';

interface Role {
  name: string;
  avatar: string;
  desc?: string;
  mafia: boolean;
}

const Roles: React.FC = () => {
  const [roles, setRoles] = useState<Role[]>([]);
  const apiUrl = import.meta.env.VITE_API_URL;
  console.log('apiUrl:', apiUrl);

  useEffect(() => {
    const getRoles = async () => {
      try {
        const roles = await fetchRoles();
        setRoles(roles);
      } catch (error) {
        console.error('Error fetching roles:', error);
      }
    };

    getRoles();
  }, []);

  return (
    <div className="flex flex-wrap justify-center">
      {roles.map((role) => (
        <div
          key={role.name}
          className="max-w-sm  bg-stormy rounded overflow-hidden shadow-lg m-4 transform transition duration-500 hover:scale-105 hover:shadow-2xl"
        >
          <img className="w-full" src={role.avatar} alt={role.name} />
          <div className="px-6 py-4">
            <div className="font-bold text-xl mb-2">{role.name}</div>
            <p className="text-gray-700 text-base">{role.desc}</p>
            <p
              className={`mt-2 font-semibold ${role.mafia ? 'text-red-500' : 'text-green-500'
                }`}
            >
              {role.mafia ? 'Mafia' : 'Not Mafia'}
            </p>
          </div>
        </div>
      ))}
    </div>
  );
};

export default Roles;
