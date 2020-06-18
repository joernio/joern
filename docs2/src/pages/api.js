import React from 'react';
import useBaseUrl from '@docusaurus/useBaseUrl';
import { RedocStandalone } from 'redoc';

function API() {
  return (
		<html>
			<head>
				<title>ShiftLeft API Reference</title>
				<meta charset="utf-8" />
				<meta name="viewport" content="width=device-width, initial-scale=1" />
				<link href="https://fonts.googleapis.com/css?family=Montserrat:300,400,700|Roboto:300,400,700" rel="stylesheet" />
			</head>
			<body>
				<RedocStandalone specUrl={useBaseUrl('/apiv4.yaml')} />
			</body>
		</html>
  );
}

export default API;
