
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"dep","title":"Row Score","type":"Variable","description":{"R":"Update this when R package is to be dealt with\n"}},{"name":"factors","title":"Categorical Pretictors","type":"Variables","suggested":["nominal"],"permitted":["factor"],"description":{"R":"Update this when R package is to be dealt with\n"}},{"name":"covs","title":"Continuous Pretictors","type":"Variables","default":null,"suggested":["continuous","ordinal"],"permitted":["numeric"],"description":{"R":"Update this when R package is to be dealt with\n"}},{"name":"covsTransformations","title":"Transformations","type":"NMXList","options":[{"name":"quadratic","title":"Quadratic"},{"name":"cubic","title":"Cubic"},{"name":"log","title":"Natural Logarithm"},{"name":"log10","title":"Logarithm base 10"},{"name":"sqrt","title":"Square root"},{"name":"Square root","title":"sqrt"},{"name":"reciprocal","title":"Reciprocal (1/y)"}],"default":["quadratic","cubic","log","sqrt"],"description":{"R":"Update this when R package is to be dealt with\n"}},{"name":"scoreTypes","title":"Scoring methods","type":"NMXList","options":[{"name":"equivalent","title":"Equivalent Scores"},{"name":"standardized","title":"Z-scores"},{"name":"percent","title":"Percentiles"}],"default":["equivalent","standardized","percent"],"description":{"R":"Update this when R package is to be dealt with\n"}},{"name":"method","title":"Selection method","type":"List","options":[{"name":"univariate","title":"Best univariate fitting"},{"name":"best","title":"Best fitting combination"}],"default":"best","description":{"R":"Update this when R package is to be dealt with\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Scoring",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Row Score",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Categorical Pretictors",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "factors",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Continuous Pretictors",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "covs",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Continous predictors",
			collapsed: true,
			stretchFactor: 1,
			style: "inline",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Transformations",
					name: "covsTransformations",
					controls: [
						{
							name: "covsTransformations_quadratic",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "quadratic"
						},
						{
							name: "covsTransformations_cubic",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "cubic"
						},
						{
							name: "covsTransformations_log",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "log"
						},
						{
							name: "covsTransformations_log10",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "log10"
						},
						{
							name: "covsTransformations_sqrt",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "sqrt"
						},
						{
							name: "covsTransformations_Square root",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "Square root"
						},
						{
							name: "covsTransformations_reciprocal",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "covsTransformations",
							optionPart: "reciprocal"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Method",
					controls: [
						{
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							name: "method_best",
							optionName: "method",
							optionPart: "best"
						},
						{
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							name: "method_univariate",
							optionName: "method",
							optionPart: "univariate"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Scoring",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Scoring Types",
					name: "scoreTypes",
					controls: [
						{
							name: "scoreTypes_equivalent",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "scoreTypes",
							optionPart: "equivalent"
						},
						{
							name: "scoreTypes_standardized",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "scoreTypes",
							optionPart: "standardized"
						},
						{
							name: "scoreTypes_percent",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionName: "scoreTypes",
							optionPart: "percent"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
