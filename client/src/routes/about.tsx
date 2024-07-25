import React from 'react';
import GitHubProfiles from '../components/GithubProfiles';

const About: React.FC = () => {
  const githubProfileUrls = [
    'https://api.github.com/users/zaranix',
    'https://api.github.com/users/yeganesalami',
    'https://api.github.com/users/sabasnd',
    'https://api.github.com/users/cyberkatze',
    'https://api.github.com/users/m3hransh',
  ];

  return (
    <div className="container mx-auto px-4 py-8">
      {/* Developer Profiles Section */}
      <section>
        <h1 className="text-3xl font-bold text-center mb-4 bg-gradient-to-r from-mauve to-lavender text-transparent bg-clip-text ">ABOUT US</h1>
        <GitHubProfiles profileUrls={githubProfileUrls} />
      </section>
    </div>
  );
};

export default About;
