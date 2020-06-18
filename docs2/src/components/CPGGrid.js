import React  from 'react';
import { useState }  from 'react';
import { useRef }  from 'react';
import { useInterval }  from 'react';
import { useEffect }  from 'react';

import * as d3 from "d3";

const highlightColor = '#E94F37';
const fadeOpacity = 0.2;

const colorForLabel = {
  META_DATA: '#004BA8',
  FILE: '#F6F7EB',
  METHOD: '#7880B5',
  TYPE_DECL: '#D1B3C4',
  NAMESPACE_BLOCK: '#B392AC',
  HIGHLIGHT: '#FF7477',
  BLOCK: '#1B998B',
  CALL: '#E15A97',
  CONTROL_STRUCTURE: '#861388',
  IDENTIFIER: '#735D78',
  LITERAL: '#E8C2CA',
  LOCAL: '#6F5E5C',
  METHOD_PARAMETER_IN: '#7C7F65',
  METHOD_PARAMETER_OUT: '#C6CAED',
  METHOD_RETURN: '#B0D0D3',
  NAMESPACE: '#266DD3',
  TAG: '#344055',
  TYPE: '#C4E7D4',

  UNKNOWN: '#F7D1CD'
}

const CPGGrid = ({ data }) => {
  const [dataset, setDataset] = useState(
    data.nodes
  )

  const ref = useRef()
  const numPerRow = 50
  const size = 14
  const pad = 2

  useEffect(() => {
    const svgElement = d3.select(ref.current)
    svgElement.selectAll("rect")
      .data(dataset)
      .enter().append("rect")
      .attr('x', (d, i) => {
        const n = i % numPerRow
        return n * (size + pad)
      })
      .attr('y', (d, i) => {
        const n = Math.floor(i / numPerRow)
        return n * (size + pad)
      })
      .attr('width', size)
      .attr('height', size)
      .style('opacity', d => {
        if (typeof(d._meta_visual_annotations) === undefined) {
          return 1.0;
        }

        const annotations = d._meta_visual_annotations;
        if (d._meta_visual_annotations.fade === true) {
          return fadeOpacity;
        }
        return 1.0;
      })
      .attr('fill', d => {
        if (typeof(d._meta_visual_annotations) === undefined) {
          return colorForLabel[d._label];
        }

        const annotations = d._meta_visual_annotations;
        if (d._meta_visual_annotations.highlight === true) {
          return highlightColor;
        }
        return colorForLabel[d._label];
      })
  }, [dataset])

  return (
    <svg
      viewBox="0 0 800 90"
      ref={ref}
    />
  )
}

export default CPGGrid;
