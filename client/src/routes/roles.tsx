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
          className="relative max-w-sm  bg-gradient-to-t from-blue to-lavender rounded overflow-hidden  shadow-lg m-4 transform transition duration-500 hover:scale-105 hover:shadow-2xl"
        >
              <div className={` ${role.mafia ? 'bg-red' : 'bg-green'} absolute w-[9ch] text-center top-3 pt-14 pb-2 left-3 angle-label text-text-inverted px-2 py-1 font-semibold rounded`}>  

              {role.mafia ? 'Mafia' : 'Citizen'}
    </div> 
          <img className="w-full" src={role.avatar} alt={role.name} />
          <div className="px-6 py-4">
            <div className=" text-text-inverted font-bold text-xl mb-2">{role.name}</div>
            <p className="text-text-inverted">{role.desc}</p>
          </div>
        </div>
      ))}
    </div>
  );
};

export default Roles;
