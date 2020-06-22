const annotated = (cpg, highlightFn, fadeFn) => {
  if (typeof(highlightFn) === undefined && typeof(fadeFn) === undefined) {
    return cpg;
  };


  // return none if the thing is not valid
  if (typeof(cpg) === undefined || typeof(cpg.nodes) === undefined) {
    return null;
  }

  // deep copy
  let annotatedCpg = JSON.parse(JSON.stringify(cpg));

  // initialize annotations dictionary
  annotatedCpg.nodes = annotatedCpg.nodes.map((val) => {
    val._meta_visual_annotations = {highlight: false, fade: false};
    return val;
  });

  if (typeof(highlightFn) !== undefined) {
    annotatedCpg.nodes = annotatedCpg.nodes.map((val) => {
      if (highlightFn(val) === true) {
        val._meta_visual_annotations.highlight = true;
      }
      return val;
    });
  }

  if (typeof(fadeFn) !== undefined) {
    annotatedCpg.nodes = annotatedCpg.nodes.map((val) => {
      if (fadeFn(val) === true) {
        val._meta_visual_annotations.fade = true;
      }
      return val;
    });
  }

  return annotatedCpg;
};

export default annotated;

