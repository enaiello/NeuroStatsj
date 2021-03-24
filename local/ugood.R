title: Scoring
name: Scoring
jus: '3.0'
stage: 0
# compilerMode: tame means that if you define something here, the compiler will leave it alone
# compilerMode: aggressive means that the compiler will force any option in "*.a.yaml" to be listed here ignoring your structure
compilerMode: tame

children:
  - type: VariableSupplier
persistentItems: false
stretchFactor: 1
children:
  - type: TargetLayoutBox
label: Row Score
children:
  - type: VariablesListBox
name: dep
maxItemCount: 1
isTarget: true
- type: TargetLayoutBox
label: Categorical Pretictors
children:
  - type: VariablesListBox
name: factors
isTarget: true
- type: TargetLayoutBox
label: Continuous Pretictors
children:
  - type: VariablesListBox
name: covs
isTarget: true
- type: LayoutBox
margin: large
children:
  - type: ComboBox
name: alt
### the following creates a foldable tab containing the options you specify as children. 
### This requires at the top "compilerMode: tame", otherwise the compiler overwrite everything
"
  - type: CollapseBox
    label: Model
    collapsed: true
    stretchFactor: 1
    children:
        - type: LayoutBox
          margin: large
          children:
                  - type: CheckBox
                    name: varEq
                  - type: Label
                    label: Transformations
                    name: covsTransformations
                    children:
                      - name: covsTransformations_quadratic
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: quadratic
                      - name: covsTransformations_cubic
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: cubic
                      - name: covsTransformations_log
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: log
                      - name: covsTransformations_log10
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: log10
                      - name: covsTransformations_sqrt
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: sqrt
                      - name: covsTransformations_Square root
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: Square root
                      - name: covsTransformations_reciprocal
                        type: CheckBox
                        optionName: covsTransformations
                        optionPart: reciprocal
