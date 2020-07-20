import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';

const useStyles = makeStyles({
  table: {
    minWidth: 550,
  },
  cell: {
    color: 'white'
  }
});

const NodeStepDetail = ({ stepInfo }) => {
  const classes = useStyles();

  const propertyDirectives = stepInfo.propertyDirectives;
  const nodeTypeSteps = stepInfo.nodeTypeSteps;
  const complexSteps = stepInfo.complexSteps;

  const noneElement = (<p style={{ fontStyle: 'italic' }}>None.</p>)

  return (
    <div className="sl-node-step-detail" style={{ borderBottom: "4px solid #f4f4f4", marginBottom: 80, padding: 6 }}>
        <h4>Supported Property Directives</h4>

        {propertyDirectives.length == 0 ? noneElement : (
          <TableContainer>
            <Table className={classes.table} aria-label="Overview of Property Directives">
              <TableHead>
                <TableRow>
                  <TableCell className={classes.cell} style={{ width: 200 }}>Property Directive</TableCell>
                  <TableCell className={classes.cell} align="left">Return Type</TableCell>
                  <TableCell className={classes.cell} align="left">Description</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {propertyDirectives.map((directive) => (
                  <TableRow key={directive.queryComponent}>
                    <TableCell className={classes.cell} component="th" scope="row">
                      {directive.queryComponent}
                    </TableCell>
                    <TableCell className={classes.cell} align="left">{directive.returnType}</TableCell>
                    <TableCell className={classes.cell} align="left">{directive.description}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </TableContainer>
        )}

        <h4>Supported Node Type Steps</h4>

        {nodeTypeSteps.length == 0 ? noneElement : (
          <TableContainer>
            <Table className={classes.table} aria-label="Overview of Node Type Steps">
              <TableHead>
                <TableRow>
                  <TableCell className={classes.cell} style={{ width: 200 }}>Node Type Step</TableCell>
                  <TableCell className={classes.cell} align="left">Description</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {nodeTypeSteps.map((step) => (
                  <TableRow key={step.queryComponent}>
                    <TableCell className={classes.cell} component="th" scope="row">
                      {step.queryComponent}
                    </TableCell>
                    <TableCell className={classes.cell} align="left">{step.description}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </TableContainer>
        )}

        <h4>Supported Complex Steps</h4>

        {complexSteps.length == 0 ? noneElement : (
          <TableContainer>
            <Table className={classes.table} aria-label="Overview of Complex Steps">
              <TableHead>
                <TableRow>
                  <TableCell className={classes.cell} style={{ width: 200 }}>Complex Step</TableCell>
                  <TableCell className={classes.cell} align="left">Description</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {complexSteps.map((step) => (
                  <TableRow key={step.queryComponent}>
                    <TableCell className={classes.cell} component="th" scope="row">
                      {step.queryComponent}
                    </TableCell>
                    <TableCell className={classes.cell} align="left">{step.description}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </TableContainer>
        )}

      </div>
  );
};

export default NodeStepDetail;

