const _ = require('lodash');
import { cpgqlRef, StepKind, StepFamily, Steps, StepConnections, PropertyDirectives, PropertyDirectivesConnections } from './cpgqlref.js';

describe('CPGQLRef', () => {
  it('implements the necessary methods', () => {
      expect(cpgqlRef.stepInfoForKind).toBeDefined();
      expect(cpgqlRef.stepsInfoForFamily).toBeDefined();
  });

  describe('.stepsInfoForFamily', () => {
    it('returns `null` for undefined step families', () => {
      const randomKey = Math.random().toString(36).slice(-10);
      expect(cpgqlRef.stepsInfoForFamily(randomKey)).toBeNull();
    });

    it('returns non-empty arrays for all step families', () => {
      const allStepFamilies = Object.keys(StepFamily);
      allStepFamilies.forEach((stepFamily) => {
        expect(cpgqlRef.stepsInfoForFamily(stepFamily)).toBeTruthy();
        expect(cpgqlRef.stepsInfoForFamily(stepFamily).length).toBeGreaterThan(0);
      });
    });
  });

  describe('.stepInfoForKind', () => {
    it('returns `null` for undefined step kinds', () => {
      const randomKey = Math.random().toString(36).slice(-10);
      expect(cpgqlRef.stepInfoForKind(randomKey)).toBeNull();
    });

    it('returns truthy values for all node type steps', () => {
      for ( const [ stepKind, step ] of Object.entries(Steps)) {
        if (step.family == StepFamily.NodeTypeStep) {
          expect(cpgqlRef.stepInfoForKind(stepKind)).toBeTruthy();
        }
      };
    });

    it('returns a valid step info object for the step kind `All`', () => {
      const step = Steps[StepKind.All];
      const result = cpgqlRef.stepInfoForKind(StepKind.All);
      expect(result.description).toBe(step.description);
      expect(result.queryComponent).toBe(step.queryComponent);
    });

    it('returns the correct number of property directives for the step kind `All`', () => {
      const connectionsForStep = PropertyDirectivesConnections[StepKind.All];
      const result = cpgqlRef.stepInfoForKind(StepKind.All);
      expect(result.propertyDirectives.length).toBe(connectionsForStep.length);
    });

    it('returns the correct number of allowed steps for the step kind `All`', () => {
      const connectionsForStep = StepConnections[StepKind.All];
      const result = cpgqlRef.stepInfoForKind(StepKind.All);
      expect(result.allowedSteps.length).toBe(connectionsForStep.length);
    });

    it('returns the correct number of allowed steps for the step kind `Method`', () => {
      const connectionsForStep = StepConnections[StepKind.Method];
      const result = cpgqlRef.stepInfoForKind(StepKind.Method);
      expect(result.allowedSteps.length).toBe(connectionsForStep.length);
    });


    it('returns a correctly set contextualDescription for the step kind `Method`', () => {
      const connectionsForStep = StepConnections[StepKind.Method];
      const stepInfo = cpgqlRef.stepInfoForKind(StepKind.Method);
      const allowedStepsDescription = {};
      stepInfo.allowedSteps.forEach((allowedStep) => {
        allowedStepsDescription[allowedStep.kind] = allowedStep.description;
      });
      connectionsForStep.forEach((connection) => {
        const allowedStepDescription = allowedStepsDescription[connection.kind];
        const stepDescription = _.clone(Steps[connection.kind].description);
        if (connection.contextualDescription != null) {
          expect(allowedStepDescription).toBe(connection.contextualDescription);
        } else if (connection.contextualDescription == null && stepDescription != null) {
          expect(allowedStepDescription).toBe(stepDescription);
        } else {
          expect(allowedStepDescription).toBeUndefined();
        }
      });
    });

    it('returns the correct number of node type steps for the step kind `Method`', () => {
      const connectionsForStep = StepConnections[StepKind.Method];
      const connectionsToSteps = connectionsForStep.map((connection) => {
        return Steps[connection.kind];
      });
      const nodeTypeStepConnections = connectionsToSteps.filter((step) => {
        return step.family == StepFamily.NodeTypeStep;
      });
      const result = cpgqlRef.stepInfoForKind(StepKind.Method);
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
      const result = cpgqlRef.stepInfoForKind(StepKind.Method);
      expect(result.complexSteps.length).toBe(complexStepConnections.length);
    });

    it('returns at least one filter step for the step kind `Method`', () => {
      const result = cpgqlRef.stepInfoForKind(StepKind.Method);
      expect(result.filterSteps.length).toBeGreaterThan(0);
    });

    it('returns at least one filter step for the step kind `Parameter`', () => {
      const result = cpgqlRef.stepInfoForKind(StepKind.Parameter);
      expect(result.filterSteps.length).toBeGreaterThan(0);
    });

    it('the returned filters step for the step kind `Method` contain the Where and Filter filters', () => {
      const result = cpgqlRef.stepInfoForKind(StepKind.Method);
      expect(result.filterSteps.map(filterStep => filterStep.kind)).toContain(StepKind.WhereFilter);
      expect(result.filterSteps.map(filterStep => filterStep.kind)).toContain(StepKind.FilterFilter);
    });

    it('the returned filters step for the step kind `Returns` contain the Where and Filter filters', () => {
      const result = cpgqlRef.stepInfoForKind(StepKind.Method);
      expect(result.filterSteps.map(filterStep => filterStep.kind)).toContain(StepKind.WhereFilter);
      expect(result.filterSteps.map(filterStep => filterStep.kind)).toContain(StepKind.FilterFilter);
    });

    it('returns the correct number of complex steps for the step kind `Returns`', () => {
      const connectionsForStep = StepConnections[StepKind.Returns];
      const connectionsToSteps = connectionsForStep.map((connection) => {
        return Steps[connection.kind];
      });
      const complexStepConnections = connectionsToSteps.filter((step) => {
        return step.family == StepFamily.ComplexStep;
      });
      const result = cpgqlRef.stepInfoForKind(StepKind.Returns);
      expect(result.complexSteps.length).toBe(complexStepConnections.length);
    });

    it('the property directives for step kind `All` contain expected properties', () => {
      const result = cpgqlRef.stepInfoForKind(StepKind.All);
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
      const result = cpgqlRef.stepInfoForKind(StepKind.All);
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
