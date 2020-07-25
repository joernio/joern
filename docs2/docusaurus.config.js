const path = require('path');

module.exports = {
  title: 'Joern Documentation',
  tagline: 'The Joern Code Analyzer',
  url: 'https://docs.joern.io',
  baseUrl: '/',
  favicon: 'img/website-icon.png',
  themeConfig: {
	    disableDarkMode:  false,
	    defaultDarkMode:  true,
    navbar: {
      title: '',
      logo: {
        alt: 'Joern Logo',
        src: 'img/website-icon.png',
        href: '/home',
        target: '_self'
      },
      links: [        
      ],
    },
    footer: {
      links: [],      
    },
    prism: {
      theme: require('prism-react-renderer/themes/github'),
      additionalLanguages: ['powershell']
    }
  },
  stylesheets: [
    'react-image-lightbox/style.css'
  ],
  plugins: [
    path.resolve(__dirname, './node_modules/docusaurus-lunr-search/')
  ],
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          routeBasePath: '',
          editUrl: '',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};

