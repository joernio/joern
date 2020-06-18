import { oqlRef, StepKind, StepFamily, Steps, StepConnections, PropertyDirectives, PropertyDirectivesConnections } from './oqlref.js';

describe('OQLRef', () => {
  it('implements the necessary methods', () => {
      expect(oqlRef.stepInfoForKind).toBeDefined();
      expect(oqlRef.stepsInfoForFamily).toBeDefined();
  });

  describe('.stepsInfoForFamily', () => {
    it('returns `null` for undefined step families', () => {
      const randomKey = Math.random().toString(36).slice(-10);
      expect(oqlRef.stepsInfoForFamily(randomKey)).toBeNull();
    });

    it('returns non-empty arrays for all step families', () => {
      const allStepFamilies = Object.keys(StepFamily);
      allStepFamilies.forEach((stepFamily) => {
        expect(oqlRef.stepsInfoForFamily(stepFamily)).toBeTruthy();
        expect(oqlRef.stepsInfoForFamily(stepFamily).length).toBeGreaterThan(0);
      });
    });
  });

  describe('.stepInfoForKind', () => {
    it('returns `null` for undefined step kinds', () => {
      const randomKey = Math.random().toString(36).slice(-10);
      expect(oqlRef.stepInfoForKind(randomKey)).toBeNull();
    });

    it('returns truthy values for all valid step kinds', () => {
      const allStepKinds = Object.keys(StepKind);
      allStepKinds.forEach((stepKind) => {
        expect(oqlRef.stepInfoForKind(stepKind)).toBeTruthy();
      });
    });

    it('returns a valid step info object for the step kind `All`', () => {
      const step = Steps[StepKind.All];
      const result = oqlRef.stepInfoForKind(StepKind.All);
      expect(result.description).toBe(step.description);
      expect(result.queryComponent).toBe(step.queryComponent);
    });

    it('returns the correct number of property directives for the step kind `All`', () => {
      const connectionsForStep = PropertyDirectivesConnections[StepKind.All];
      const result = oqlRef.stepInfoForKind(StepKind.All);
      expect(result.propertyDirectives.length).toBe(connectionsForStep.length);
    });

    it('returns the correct number of allowed steps for the step kind `All`', () => {
      const connectionsForStep = StepConnections[StepKind.All];
      const result = oqlRef.stepInfoForKind(StepKind.All);
      expect(result.allowedSteps.length).toBe(connectionsForStep.length);
    });

    it('returns the correct number of allowed steps for the step kind `Method`', () => {
      const connectionsForStep = StepConnections[StepKind.Method];
      const result = oqlRef.stepInfoForKind(StepKind.Method);
      expect(result.allowedSteps.length).toBe(connectionsForStep.length);
    });

    it('returns the correct number of node type steps for the step kind `Method`', () => {
      const connectionsForStep = StepConnections[StepKind.Method];
      const connectionsToSteps = connectionsForStep.map((connection) => {
        return Steps[connection.kind];
      });
      const nodeTypeStepConnections = connectionsToSteps.filter((step) => {
        return step.family == StepFamily.NodeTypeStep;
      });
      const result = oqlRef.stepInfoForKind(StepKind.Method);
      expect(result.nodeTypeSteps.length).toBe(nodeTypeStepConnections.length);
    });

    it('returns the correct number of complex steps for the step kind `Method`', () => {
      const connectionsForStep = StepConnections[StepKind.Method];
      const connectionsToSteps = connectionsForStep.map((connection) => {
        return Steps[connection.kind];
      });
      const complexStepConnections = connectionsToSteps.filter((step) => {
        return step.family == StepFamily.ComplexStep;
      });
      const result = oqlRef.stepInfoForKind(StepKind.Method);
      expect(result.complexSteps.length).toBe(complexStepConnections.length);
    });


    it('returns the correct number of complex steps for the step kind `Returns`', () => {
      const connectionsForStep = StepConnections[StepKind.Returns];
      const connectionsToSteps = connectionsForStep.map((connection) => {
        return Steps[connection.kind];
      });
      const complexStepConnections = connectionsToSteps.filter((step) => {
        return step.family == StepFamily.ComplexStep;
      });
      const result = oqlRef.stepInfoForKind(StepKind.Returns);
      expect(result.complexSteps.length).toBe(complexStepConnections.length);
    });

    it('the property directives for step kind `All` contain expected properties', () => {
      const result = oqlRef.stepInfoForKind(StepKind.All);
      result.propertyDirectives.forEach((propertyDirective) => {
        expect(propertyDirective).toHaveProperty('queryComponent');
        expect(propertyDirective).toHaveProperty('description');
        expect(propertyDirective).toHaveProperty('returnType');

        expect(propertyDirective.queryComponent).toBeDefined();
        expect(propertyDirective.returnType).toBeDefined();
        expect(propertyDirective.description).toBeDefined();
      });
    });

    it('the allowed steps for step kind `All` contain expected properties', () => {
      const result = oqlRef.stepInfoForKind(StepKind.All);
      result.allowedSteps.forEach((step) => {
        expect(step).toHaveProperty('queryComponent');
        expect(step).toHaveProperty('family');
        expect(step).toHaveProperty('description');
        expect(step).toHaveProperty('sourceBasedOnly');

        expect(step.queryComponent).toBeDefined();
        expect(step.family).toBeDefined();
        expect(step.description).toBeDefined();
        expect(step.sourceBasedOnly).toBeDefined();
      });
    });
  });
});
