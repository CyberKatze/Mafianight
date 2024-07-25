import React, { useEffect, useState } from 'react';  
import axios from 'axios';  
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';  
import { faGithub } from '@fortawesome/free-brands-svg-icons';  

interface GitHubProfile {  
  avatar_url: string;  
  login: string;  
  bio: string | null;  
  html_url: string;  
}  

interface GitHubProfileListProps {  
  profileUrls: string[];  
}  

const apiUrl = import.meta.env.VITE_API_URL;
// Fetch GitHub profile from the backend  
const fetchGitHubProfile = async (username: string) => {  
  const response = await axios.get<GitHubProfile>(`${apiUrl}/github-profile/${username}`);  
  return response.data;  
};  

const GithubProfiles: React.FC<GitHubProfileListProps> = ({ profileUrls }) => {  
  const [profiles, setProfiles] = useState<GitHubProfile[]>([]);  
  const [loading, setLoading] = useState<boolean>(true);  
  const [error, setError] = useState<string | null>(null);  

  useEffect(() => {  
    const fetchProfiles = async () => {  
      try {  
        const profileUsernames = profileUrls.map(url => url.split('/').pop()!);  
        const profilePromises = profileUsernames.map(username => fetchGitHubProfile(username));  
        const profilesData = await Promise.all(profilePromises);  
        setProfiles(profilesData);  
      } catch (err: any) {  
        setError(err.message);  
      } finally {  
        setLoading(false);  
      }  
    };  

    fetchProfiles();  
  }, [profileUrls]);  

  if (loading) return <div>Loading...</div>;  
  if (error) return <div>Error: {error}</div>;  

  return (  
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">  
      {profiles.map(profile => (  
        <a  
          key={profile.login}  
          href={profile.html_url}  
          target="_blank"  
          rel="noopener noreferrer"  
          className="relative bg-gradient-to-b from-mantle to-crust rounded-lg shadow-md p-4 transform transition-transform hover:scale-105"  
        >  
          <img  
            src={profile.avatar_url}  
            alt={`Avatar of ${profile.login}`}  
            className="w-24 h-24 rounded-full mx-auto mb-4"  
          />  
          <h2  
            className="text-center font-bold mb-4 text-mauve"  
          >  
            {profile.login}  
          </h2>  
          <p className="text-default text-center text-subtext-muted mb-2">  
            {profile.bio ?? 'No bio available'}  
          </p>  
          <FontAwesomeIcon icon={faGithub} className="absolute top-4 right-4 text-4xl text-sky" />  
        </a>  
      ))}  
    </div>  
  );  
};  

export default GithubProfiles;
