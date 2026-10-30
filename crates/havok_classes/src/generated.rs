/// Reduce the burden of individual imports by importing a set of types needed to create a havok class structure here.
mod class_requires {
    pub use havok_serde::{
        ser::{Error as _, SerializeFlags, SerializeStruct, Serializer, TypeSize},
        de::{self, Error as _, Deserializer},
        HavokClass,
    };
    pub use havok_types::*;
    #[cfg(feature = "json_schema")]
    pub fn make_large_int_array_schema(
        generator: &mut schemars::SchemaGenerator,
    ) -> schemars::Schema {
        let mut schema = <Vec<i32> as schemars::JsonSchema>::json_schema(generator);
        let mut map = schema.ensure_object();
        schema
    }
}
pub mod BGSGamebryoSequenceGenerator_;
pub use BGSGamebryoSequenceGenerator_::*;
pub mod BSBoneSwitchGenerator_;
pub use BSBoneSwitchGenerator_::*;
pub mod BSBoneSwitchGeneratorBoneData_;
pub use BSBoneSwitchGeneratorBoneData_::*;
pub mod BSComputeAddBoneAnimModifier_;
pub use BSComputeAddBoneAnimModifier_::*;
pub mod BSCyclicBlendTransitionGenerator_;
pub use BSCyclicBlendTransitionGenerator_::*;
pub mod BSDecomposeVectorModifier_;
pub use BSDecomposeVectorModifier_::*;
pub mod BSDirectAtModifier_;
pub use BSDirectAtModifier_::*;
pub mod BSDistTriggerModifier_;
pub use BSDistTriggerModifier_::*;
pub mod BSEventEveryNEventsModifier_;
pub use BSEventEveryNEventsModifier_::*;
pub mod BSEventOnDeactivateModifier_;
pub use BSEventOnDeactivateModifier_::*;
pub mod BSEventOnFalseToTrueModifier_;
pub use BSEventOnFalseToTrueModifier_::*;
pub mod BSGetTimeStepModifier_;
pub use BSGetTimeStepModifier_::*;
pub mod BSInterpValueModifier_;
pub use BSInterpValueModifier_::*;
pub mod BSIsActiveModifier_;
pub use BSIsActiveModifier_::*;
pub mod BSIStateManagerModifier_;
pub use BSIStateManagerModifier_::*;
pub mod BSIStateManagerModifierBSiStateData_;
pub use BSIStateManagerModifierBSiStateData_::*;
pub mod BSIStateManagerModifierBSIStateManagerStateListener_;
pub use BSIStateManagerModifierBSIStateManagerStateListener_::*;
pub mod BSiStateTaggingGenerator_;
pub use BSiStateTaggingGenerator_::*;
pub mod BSLimbIKModifier_;
pub use BSLimbIKModifier_::*;
pub mod BSLookAtModifier_;
pub use BSLookAtModifier_::*;
pub mod BSLookAtModifierBoneData_;
pub use BSLookAtModifierBoneData_::*;
pub mod BSModifyOnceModifier_;
pub use BSModifyOnceModifier_::*;
pub mod BSOffsetAnimationGenerator_;
pub use BSOffsetAnimationGenerator_::*;
pub mod BSPassByTargetTriggerModifier_;
pub use BSPassByTargetTriggerModifier_::*;
pub mod BSRagdollContactListenerModifier_;
pub use BSRagdollContactListenerModifier_::*;
pub mod BSSpeedSamplerModifier_;
pub use BSSpeedSamplerModifier_::*;
pub mod BSSynchronizedClipGenerator_;
pub use BSSynchronizedClipGenerator_::*;
pub mod BSTimerModifier_;
pub use BSTimerModifier_::*;
pub mod BSTweenerModifier_;
pub use BSTweenerModifier_::*;
pub mod hkAabb_;
pub use hkAabb_::*;
pub mod hkAabbHalf_;
pub use hkAabbHalf_::*;
pub mod hkAabbUint32_;
pub use hkAabbUint32_::*;
pub mod hkaAnimatedReferenceFrame_;
pub use hkaAnimatedReferenceFrame_::*;
pub mod hkaAnimation_;
pub use hkaAnimation_::*;
pub mod hkaAnimationBinding_;
pub use hkaAnimationBinding_::*;
pub mod hkaAnimationContainer_;
pub use hkaAnimationContainer_::*;
pub mod hkaAnimationPreviewColorContainer_;
pub use hkaAnimationPreviewColorContainer_::*;
pub mod hkaAnnotationTrack_;
pub use hkaAnnotationTrack_::*;
pub mod hkaAnnotationTrackAnnotation_;
pub use hkaAnnotationTrackAnnotation_::*;
pub mod hkaBone_;
pub use hkaBone_::*;
pub mod hkaBoneAttachment_;
pub use hkaBoneAttachment_::*;
pub mod hkaDefaultAnimatedReferenceFrame_;
pub use hkaDefaultAnimatedReferenceFrame_::*;
pub mod hkaDeltaCompressedAnimation_;
pub use hkaDeltaCompressedAnimation_::*;
pub mod hkaDeltaCompressedAnimationQuantizationFormat_;
pub use hkaDeltaCompressedAnimationQuantizationFormat_::*;
pub mod hkaFootstepAnalysisInfo_;
pub use hkaFootstepAnalysisInfo_::*;
pub mod hkaFootstepAnalysisInfoContainer_;
pub use hkaFootstepAnalysisInfoContainer_::*;
pub mod hkaInterleavedUncompressedAnimation_;
pub use hkaInterleavedUncompressedAnimation_::*;
pub mod hkaKeyFrameHierarchyUtility_;
pub use hkaKeyFrameHierarchyUtility_::*;
pub mod hkaKeyFrameHierarchyUtilityControlData_;
pub use hkaKeyFrameHierarchyUtilityControlData_::*;
pub mod hkAlignSceneToNodeOptions_;
pub use hkAlignSceneToNodeOptions_::*;
pub mod hkaMeshBinding_;
pub use hkaMeshBinding_::*;
pub mod hkaMeshBindingMapping_;
pub use hkaMeshBindingMapping_::*;
pub mod hkaQuantizedAnimation_;
pub use hkaQuantizedAnimation_::*;
pub mod hkaQuantizedAnimationTrackCompressionParams_;
pub use hkaQuantizedAnimationTrackCompressionParams_::*;
pub mod hkaRagdollInstance_;
pub use hkaRagdollInstance_::*;
pub mod hkArrayTypeAttribute_;
pub use hkArrayTypeAttribute_::*;
pub mod hkaSkeleton_;
pub use hkaSkeleton_::*;
pub mod hkaSkeletonLocalFrameOnBone_;
pub use hkaSkeletonLocalFrameOnBone_::*;
pub mod hkaSkeletonMapper_;
pub use hkaSkeletonMapper_::*;
pub mod hkaSkeletonMapperData_;
pub use hkaSkeletonMapperData_::*;
pub mod hkaSkeletonMapperDataChainMapping_;
pub use hkaSkeletonMapperDataChainMapping_::*;
pub mod hkaSkeletonMapperDataSimpleMapping_;
pub use hkaSkeletonMapperDataSimpleMapping_::*;
pub mod hkaSplineCompressedAnimation_;
pub use hkaSplineCompressedAnimation_::*;
pub mod hkaSplineCompressedAnimationAnimationCompressionParams_;
pub use hkaSplineCompressedAnimationAnimationCompressionParams_::*;
pub mod hkaSplineCompressedAnimationTrackCompressionParams_;
pub use hkaSplineCompressedAnimationTrackCompressionParams_::*;
pub mod hkaWaveletCompressedAnimation_;
pub use hkaWaveletCompressedAnimation_::*;
pub mod hkaWaveletCompressedAnimationCompressionParams_;
pub use hkaWaveletCompressedAnimationCompressionParams_::*;
pub mod hkaWaveletCompressedAnimationQuantizationFormat_;
pub use hkaWaveletCompressedAnimationQuantizationFormat_::*;
pub mod hkBaseObject_;
pub use hkBaseObject_::*;
pub mod hkbAttachmentModifier_;
pub use hkbAttachmentModifier_::*;
pub mod hkbAttachmentSetup_;
pub use hkbAttachmentSetup_::*;
pub mod hkbAttributeModifier_;
pub use hkbAttributeModifier_::*;
pub mod hkbAttributeModifierAssignment_;
pub use hkbAttributeModifierAssignment_::*;
pub mod hkbAuxiliaryNodeInfo_;
pub use hkbAuxiliaryNodeInfo_::*;
pub mod hkbBehaviorEventsInfo_;
pub use hkbBehaviorEventsInfo_::*;
pub mod hkbBehaviorGraph_;
pub use hkbBehaviorGraph_::*;
pub mod hkbBehaviorGraphData_;
pub use hkbBehaviorGraphData_::*;
pub mod hkbBehaviorGraphInternalState_;
pub use hkbBehaviorGraphInternalState_::*;
pub mod hkbBehaviorGraphInternalStateInfo_;
pub use hkbBehaviorGraphInternalStateInfo_::*;
pub mod hkbBehaviorGraphStringData_;
pub use hkbBehaviorGraphStringData_::*;
pub mod hkbBehaviorInfo_;
pub use hkbBehaviorInfo_::*;
pub mod hkbBehaviorInfoIdToNamePair_;
pub use hkbBehaviorInfoIdToNamePair_::*;
pub mod hkbBehaviorReferenceGenerator_;
pub use hkbBehaviorReferenceGenerator_::*;
pub mod hkbBindable_;
pub use hkbBindable_::*;
pub mod hkbBlendCurveUtils_;
pub use hkbBlendCurveUtils_::*;
pub mod hkbBlenderGenerator_;
pub use hkbBlenderGenerator_::*;
pub mod hkbBlenderGeneratorChild_;
pub use hkbBlenderGeneratorChild_::*;
pub mod hkbBlenderGeneratorChildInternalState_;
pub use hkbBlenderGeneratorChildInternalState_::*;
pub mod hkbBlenderGeneratorInternalState_;
pub use hkbBlenderGeneratorInternalState_::*;
pub mod hkbBlendingTransitionEffect_;
pub use hkbBlendingTransitionEffect_::*;
pub mod hkbBlendingTransitionEffectInternalState_;
pub use hkbBlendingTransitionEffectInternalState_::*;
pub mod hkbBoneIndexArray_;
pub use hkbBoneIndexArray_::*;
pub mod hkbBoneWeightArray_;
pub use hkbBoneWeightArray_::*;
pub mod hkbBoolVariableSequencedData_;
pub use hkbBoolVariableSequencedData_::*;
pub mod hkbBoolVariableSequencedDataSample_;
pub use hkbBoolVariableSequencedDataSample_::*;
pub mod hkbCameraShakeEventPayload_;
pub use hkbCameraShakeEventPayload_::*;
pub mod hkbCharacter_;
pub use hkbCharacter_::*;
pub mod hkbCharacterAddedInfo_;
pub use hkbCharacterAddedInfo_::*;
pub mod hkbCharacterControlCommand_;
pub use hkbCharacterControlCommand_::*;
pub mod hkbCharacterControllerControlData_;
pub use hkbCharacterControllerControlData_::*;
pub mod hkbCharacterControllerModifier_;
pub use hkbCharacterControllerModifier_::*;
pub mod hkbCharacterControllerModifierInternalState_;
pub use hkbCharacterControllerModifierInternalState_::*;
pub mod hkbCharacterData_;
pub use hkbCharacterData_::*;
pub mod hkbCharacterDataCharacterControllerInfo_;
pub use hkbCharacterDataCharacterControllerInfo_::*;
pub mod hkbCharacterInfo_;
pub use hkbCharacterInfo_::*;
pub mod hkbCharacterSetup_;
pub use hkbCharacterSetup_::*;
pub mod hkbCharacterSkinInfo_;
pub use hkbCharacterSkinInfo_::*;
pub mod hkbCharacterSteppedInfo_;
pub use hkbCharacterSteppedInfo_::*;
pub mod hkbCharacterStringData_;
pub use hkbCharacterStringData_::*;
pub mod hkbClientCharacterState_;
pub use hkbClientCharacterState_::*;
pub mod hkbClipGenerator_;
pub use hkbClipGenerator_::*;
pub mod hkbClipGeneratorEcho_;
pub use hkbClipGeneratorEcho_::*;
pub mod hkbClipGeneratorInternalState_;
pub use hkbClipGeneratorInternalState_::*;
pub mod hkbClipTrigger_;
pub use hkbClipTrigger_::*;
pub mod hkbClipTriggerArray_;
pub use hkbClipTriggerArray_::*;
pub mod hkbCombineTransformsModifier_;
pub use hkbCombineTransformsModifier_::*;
pub mod hkbCombineTransformsModifierInternalState_;
pub use hkbCombineTransformsModifierInternalState_::*;
pub mod hkbCompiledExpressionSet_;
pub use hkbCompiledExpressionSet_::*;
pub mod hkbCompiledExpressionSetToken_;
pub use hkbCompiledExpressionSetToken_::*;
pub mod hkbComputeDirectionModifier_;
pub use hkbComputeDirectionModifier_::*;
pub mod hkbComputeDirectionModifierInternalState_;
pub use hkbComputeDirectionModifierInternalState_::*;
pub mod hkbComputeRotationFromAxisAngleModifier_;
pub use hkbComputeRotationFromAxisAngleModifier_::*;
pub mod hkbComputeRotationFromAxisAngleModifierInternalState_;
pub use hkbComputeRotationFromAxisAngleModifierInternalState_::*;
pub mod hkbComputeRotationToTargetModifier_;
pub use hkbComputeRotationToTargetModifier_::*;
pub mod hkbComputeRotationToTargetModifierInternalState_;
pub use hkbComputeRotationToTargetModifierInternalState_::*;
pub mod hkbCondition_;
pub use hkbCondition_::*;
pub mod hkbContext_;
pub use hkbContext_::*;
pub mod hkbDampingModifier_;
pub use hkbDampingModifier_::*;
pub mod hkbDampingModifierInternalState_;
pub use hkbDampingModifierInternalState_::*;
pub mod hkbDefaultMessageLog_;
pub use hkbDefaultMessageLog_::*;
pub mod hkbDelayedModifier_;
pub use hkbDelayedModifier_::*;
pub mod hkbDelayedModifierInternalState_;
pub use hkbDelayedModifierInternalState_::*;
pub mod hkbDetectCloseToGroundModifier_;
pub use hkbDetectCloseToGroundModifier_::*;
pub mod hkbDetectCloseToGroundModifierInternalState_;
pub use hkbDetectCloseToGroundModifierInternalState_::*;
pub mod hkbEvaluateExpressionModifier_;
pub use hkbEvaluateExpressionModifier_::*;
pub mod hkbEvaluateExpressionModifierInternalExpressionData_;
pub use hkbEvaluateExpressionModifierInternalExpressionData_::*;
pub mod hkbEvaluateExpressionModifierInternalState_;
pub use hkbEvaluateExpressionModifierInternalState_::*;
pub mod hkbEvaluateHandleModifier_;
pub use hkbEvaluateHandleModifier_::*;
pub mod hkbEvent_;
pub use hkbEvent_::*;
pub mod hkbEventBase_;
pub use hkbEventBase_::*;
pub mod hkbEventDrivenModifier_;
pub use hkbEventDrivenModifier_::*;
pub mod hkbEventDrivenModifierInternalState_;
pub use hkbEventDrivenModifierInternalState_::*;
pub mod hkbEventInfo_;
pub use hkbEventInfo_::*;
pub mod hkbEventPayload_;
pub use hkbEventPayload_::*;
pub mod hkbEventPayloadList_;
pub use hkbEventPayloadList_::*;
pub mod hkbEventProperty_;
pub use hkbEventProperty_::*;
pub mod hkbEventRaisedInfo_;
pub use hkbEventRaisedInfo_::*;
pub mod hkbEventRangeData_;
pub use hkbEventRangeData_::*;
pub mod hkbEventRangeDataArray_;
pub use hkbEventRangeDataArray_::*;
pub mod hkbEventSequencedData_;
pub use hkbEventSequencedData_::*;
pub mod hkbEventSequencedDataSequencedEvent_;
pub use hkbEventSequencedDataSequencedEvent_::*;
pub mod hkbEventsFromRangeModifier_;
pub use hkbEventsFromRangeModifier_::*;
pub mod hkbEventsFromRangeModifierInternalState_;
pub use hkbEventsFromRangeModifierInternalState_::*;
pub mod hkbExpressionCondition_;
pub use hkbExpressionCondition_::*;
pub mod hkbExpressionData_;
pub use hkbExpressionData_::*;
pub mod hkbExpressionDataArray_;
pub use hkbExpressionDataArray_::*;
pub mod hkbExtractRagdollPoseModifier_;
pub use hkbExtractRagdollPoseModifier_::*;
pub mod hkbFootIkControlData_;
pub use hkbFootIkControlData_::*;
pub mod hkbFootIkControlsModifier_;
pub use hkbFootIkControlsModifier_::*;
pub mod hkbFootIkControlsModifierLeg_;
pub use hkbFootIkControlsModifierLeg_::*;
pub mod hkbFootIkDriverInfo_;
pub use hkbFootIkDriverInfo_::*;
pub mod hkbFootIkDriverInfoLeg_;
pub use hkbFootIkDriverInfoLeg_::*;
pub mod hkbFootIkGains_;
pub use hkbFootIkGains_::*;
pub mod hkbFootIkModifier_;
pub use hkbFootIkModifier_::*;
pub mod hkbFootIkModifierInternalLegData_;
pub use hkbFootIkModifierInternalLegData_::*;
pub mod hkbFootIkModifierLeg_;
pub use hkbFootIkModifierLeg_::*;
pub mod hkbGenerator_;
pub use hkbGenerator_::*;
pub mod hkbGeneratorOutputListener_;
pub use hkbGeneratorOutputListener_::*;
pub mod hkbGeneratorSyncInfo_;
pub use hkbGeneratorSyncInfo_::*;
pub mod hkbGeneratorSyncInfoSyncPoint_;
pub use hkbGeneratorSyncInfoSyncPoint_::*;
pub mod hkbGeneratorTransitionEffect_;
pub use hkbGeneratorTransitionEffect_::*;
pub mod hkbGeneratorTransitionEffectInternalState_;
pub use hkbGeneratorTransitionEffectInternalState_::*;
pub mod hkbGetHandleOnBoneModifier_;
pub use hkbGetHandleOnBoneModifier_::*;
pub mod hkbGetUpModifier_;
pub use hkbGetUpModifier_::*;
pub mod hkbGetUpModifierInternalState_;
pub use hkbGetUpModifierInternalState_::*;
pub mod hkbGetWorldFromModelModifier_;
pub use hkbGetWorldFromModelModifier_::*;
pub mod hkbGetWorldFromModelModifierInternalState_;
pub use hkbGetWorldFromModelModifierInternalState_::*;
pub mod hkbHandIkControlData_;
pub use hkbHandIkControlData_::*;
pub mod hkbHandIkControlsModifier_;
pub use hkbHandIkControlsModifier_::*;
pub mod hkbHandIkControlsModifierHand_;
pub use hkbHandIkControlsModifierHand_::*;
pub mod hkbHandIkDriverInfo_;
pub use hkbHandIkDriverInfo_::*;
pub mod hkbHandIkDriverInfoHand_;
pub use hkbHandIkDriverInfoHand_::*;
pub mod hkbHandIkModifier_;
pub use hkbHandIkModifier_::*;
pub mod hkbHandIkModifierHand_;
pub use hkbHandIkModifierHand_::*;
pub mod hkbHandle_;
pub use hkbHandle_::*;
pub mod hkbIntEventPayload_;
pub use hkbIntEventPayload_::*;
pub mod hkbIntVariableSequencedData_;
pub use hkbIntVariableSequencedData_::*;
pub mod hkbIntVariableSequencedDataSample_;
pub use hkbIntVariableSequencedDataSample_::*;
pub mod hkBitField_;
pub use hkBitField_::*;
pub mod hkbKeyframeBonesModifier_;
pub use hkbKeyframeBonesModifier_::*;
pub mod hkbKeyframeBonesModifierKeyframeInfo_;
pub use hkbKeyframeBonesModifierKeyframeInfo_::*;
pub mod hkbLinkedSymbolInfo_;
pub use hkbLinkedSymbolInfo_::*;
pub mod hkbLookAtModifier_;
pub use hkbLookAtModifier_::*;
pub mod hkbLookAtModifierInternalState_;
pub use hkbLookAtModifierInternalState_::*;
pub mod hkbManualSelectorGenerator_;
pub use hkbManualSelectorGenerator_::*;
pub mod hkbManualSelectorGeneratorInternalState_;
pub use hkbManualSelectorGeneratorInternalState_::*;
pub mod hkbMessageLog_;
pub use hkbMessageLog_::*;
pub mod hkbMirroredSkeletonInfo_;
pub use hkbMirroredSkeletonInfo_::*;
pub mod hkbMirrorModifier_;
pub use hkbMirrorModifier_::*;
pub mod hkbModifier_;
pub use hkbModifier_::*;
pub mod hkbModifierGenerator_;
pub use hkbModifierGenerator_::*;
pub mod hkbModifierList_;
pub use hkbModifierList_::*;
pub mod hkbModifierWrapper_;
pub use hkbModifierWrapper_::*;
pub mod hkbMoveCharacterModifier_;
pub use hkbMoveCharacterModifier_::*;
pub mod hkbMoveCharacterModifierInternalState_;
pub use hkbMoveCharacterModifierInternalState_::*;
pub mod hkbNamedEventPayload_;
pub use hkbNamedEventPayload_::*;
pub mod hkbNamedIntEventPayload_;
pub use hkbNamedIntEventPayload_::*;
pub mod hkbNamedRealEventPayload_;
pub use hkbNamedRealEventPayload_::*;
pub mod hkbNamedStringEventPayload_;
pub use hkbNamedStringEventPayload_::*;
pub mod hkbNode_;
pub use hkbNode_::*;
pub mod hkbNodeInternalStateInfo_;
pub use hkbNodeInternalStateInfo_::*;
pub mod hkbParticleSystemEventPayload_;
pub use hkbParticleSystemEventPayload_::*;
pub mod hkbPoseMatchingGenerator_;
pub use hkbPoseMatchingGenerator_::*;
pub mod hkbPoseMatchingGeneratorInternalState_;
pub use hkbPoseMatchingGeneratorInternalState_::*;
pub mod hkbPoweredRagdollControlData_;
pub use hkbPoweredRagdollControlData_::*;
pub mod hkbPoweredRagdollControlsModifier_;
pub use hkbPoweredRagdollControlsModifier_::*;
pub mod hkbProjectData_;
pub use hkbProjectData_::*;
pub mod hkbProjectStringData_;
pub use hkbProjectStringData_::*;
pub mod hkbProxyModifier_;
pub use hkbProxyModifier_::*;
pub mod hkbProxyModifierProxyInfo_;
pub use hkbProxyModifierProxyInfo_::*;
pub mod hkbRaiseEventCommand_;
pub use hkbRaiseEventCommand_::*;
pub mod hkbRealEventPayload_;
pub use hkbRealEventPayload_::*;
pub mod hkbRealVariableSequencedData_;
pub use hkbRealVariableSequencedData_::*;
pub mod hkbRealVariableSequencedDataSample_;
pub use hkbRealVariableSequencedDataSample_::*;
pub mod hkbReferencePoseGenerator_;
pub use hkbReferencePoseGenerator_::*;
pub mod hkbRegisteredGenerator_;
pub use hkbRegisteredGenerator_::*;
pub mod hkbRigidBodyRagdollControlData_;
pub use hkbRigidBodyRagdollControlData_::*;
pub mod hkbRigidBodyRagdollControlsModifier_;
pub use hkbRigidBodyRagdollControlsModifier_::*;
pub mod hkbRoleAttribute_;
pub use hkbRoleAttribute_::*;
pub mod hkbRotateCharacterModifier_;
pub use hkbRotateCharacterModifier_::*;
pub mod hkbRotateCharacterModifierInternalState_;
pub use hkbRotateCharacterModifierInternalState_::*;
pub mod hkbSenseHandleModifier_;
pub use hkbSenseHandleModifier_::*;
pub mod hkbSenseHandleModifierRange_;
pub use hkbSenseHandleModifierRange_::*;
pub mod hkbSequence_;
pub use hkbSequence_::*;
pub mod hkbSequencedData_;
pub use hkbSequencedData_::*;
pub mod hkbSequenceInternalState_;
pub use hkbSequenceInternalState_::*;
pub mod hkbSequenceStringData_;
pub use hkbSequenceStringData_::*;
pub mod hkbSetBehaviorCommand_;
pub use hkbSetBehaviorCommand_::*;
pub mod hkbSetLocalTimeOfClipGeneratorCommand_;
pub use hkbSetLocalTimeOfClipGeneratorCommand_::*;
pub mod hkbSetNodePropertyCommand_;
pub use hkbSetNodePropertyCommand_::*;
pub mod hkbSetWordVariableCommand_;
pub use hkbSetWordVariableCommand_::*;
pub mod hkbSetWorldFromModelModifier_;
pub use hkbSetWorldFromModelModifier_::*;
pub mod hkbSimulationControlCommand_;
pub use hkbSimulationControlCommand_::*;
pub mod hkbSimulationStateInfo_;
pub use hkbSimulationStateInfo_::*;
pub mod hkbStateChooser_;
pub use hkbStateChooser_::*;
pub mod hkbStateListener_;
pub use hkbStateListener_::*;
pub mod hkbStateMachine_;
pub use hkbStateMachine_::*;
pub mod hkbStateMachineActiveTransitionInfo_;
pub use hkbStateMachineActiveTransitionInfo_::*;
pub mod hkbStateMachineDelayedTransitionInfo_;
pub use hkbStateMachineDelayedTransitionInfo_::*;
pub mod hkbStateMachineEventPropertyArray_;
pub use hkbStateMachineEventPropertyArray_::*;
pub mod hkbStateMachineInternalState_;
pub use hkbStateMachineInternalState_::*;
pub mod hkbStateMachineNestedStateMachineData_;
pub use hkbStateMachineNestedStateMachineData_::*;
pub mod hkbStateMachineProspectiveTransitionInfo_;
pub use hkbStateMachineProspectiveTransitionInfo_::*;
pub mod hkbStateMachineStateInfo_;
pub use hkbStateMachineStateInfo_::*;
pub mod hkbStateMachineTimeInterval_;
pub use hkbStateMachineTimeInterval_::*;
pub mod hkbStateMachineTransitionInfo_;
pub use hkbStateMachineTransitionInfo_::*;
pub mod hkbStateMachineTransitionInfoArray_;
pub use hkbStateMachineTransitionInfoArray_::*;
pub mod hkbStateMachineTransitionInfoReference_;
pub use hkbStateMachineTransitionInfoReference_::*;
pub mod hkbStringCondition_;
pub use hkbStringCondition_::*;
pub mod hkbStringEventPayload_;
pub use hkbStringEventPayload_::*;
pub mod hkbTestStateChooser_;
pub use hkbTestStateChooser_::*;
pub mod hkbTimerModifier_;
pub use hkbTimerModifier_::*;
pub mod hkbTimerModifierInternalState_;
pub use hkbTimerModifierInternalState_::*;
pub mod hkbTransformVectorModifier_;
pub use hkbTransformVectorModifier_::*;
pub mod hkbTransformVectorModifierInternalState_;
pub use hkbTransformVectorModifierInternalState_::*;
pub mod hkbTransitionEffect_;
pub use hkbTransitionEffect_::*;
pub mod hkbTwistModifier_;
pub use hkbTwistModifier_::*;
pub mod hkbVariableBindingSet_;
pub use hkbVariableBindingSet_::*;
pub mod hkbVariableBindingSetBinding_;
pub use hkbVariableBindingSetBinding_::*;
pub mod hkbVariableInfo_;
pub use hkbVariableInfo_::*;
pub mod hkbVariableValue_;
pub use hkbVariableValue_::*;
pub mod hkbVariableValueSet_;
pub use hkbVariableValueSet_::*;
pub mod hkbWorldEnums_;
pub use hkbWorldEnums_::*;
pub mod hkbWorldFromModelModeData_;
pub use hkbWorldFromModelModeData_::*;
pub mod hkClass_;
pub use hkClass_::*;
pub mod hkClassEnum_;
pub use hkClassEnum_::*;
pub mod hkClassEnumItem_;
pub use hkClassEnumItem_::*;
pub mod hkClassMember_;
pub use hkClassMember_::*;
pub mod hkColor_;
pub use hkColor_::*;
pub mod hkContactPoint_;
pub use hkContactPoint_::*;
pub mod hkContactPointMaterial_;
pub use hkContactPointMaterial_::*;
pub mod hkCustomAttributes_;
pub use hkCustomAttributes_::*;
pub mod hkCustomAttributesAttribute_;
pub use hkCustomAttributesAttribute_::*;
pub mod hkDataObjectTypeAttribute_;
pub use hkDataObjectTypeAttribute_::*;
pub mod hkDescriptionAttribute_;
pub use hkDescriptionAttribute_::*;
pub mod hkDocumentationAttribute_;
pub use hkDocumentationAttribute_::*;
pub mod hkGeometry_;
pub use hkGeometry_::*;
pub mod hkGeometryTriangle_;
pub use hkGeometryTriangle_::*;
pub mod hkGizmoAttribute_;
pub use hkGizmoAttribute_::*;
pub mod hkHalf8_;
pub use hkHalf8_::*;
pub mod hkIndexedTransformSet_;
pub use hkIndexedTransformSet_::*;
pub mod hkLinkAttribute_;
pub use hkLinkAttribute_::*;
pub mod hkLocalFrame_;
pub use hkLocalFrame_::*;
pub mod hkLocalFrameGroup_;
pub use hkLocalFrameGroup_::*;
pub mod hkMemoryMeshBody_;
pub use hkMemoryMeshBody_::*;
pub mod hkMemoryMeshMaterial_;
pub use hkMemoryMeshMaterial_::*;
pub mod hkMemoryMeshShape_;
pub use hkMemoryMeshShape_::*;
pub mod hkMemoryMeshTexture_;
pub use hkMemoryMeshTexture_::*;
pub mod hkMemoryMeshVertexBuffer_;
pub use hkMemoryMeshVertexBuffer_::*;
pub mod hkMemoryResourceContainer_;
pub use hkMemoryResourceContainer_::*;
pub mod hkMemoryResourceHandle_;
pub use hkMemoryResourceHandle_::*;
pub mod hkMemoryResourceHandleExternalLink_;
pub use hkMemoryResourceHandleExternalLink_::*;
pub mod hkMemoryTrackerAttribute_;
pub use hkMemoryTrackerAttribute_::*;
pub mod hkMeshBody_;
pub use hkMeshBody_::*;
pub mod hkMeshBoneIndexMapping_;
pub use hkMeshBoneIndexMapping_::*;
pub mod hkMeshMaterial_;
pub use hkMeshMaterial_::*;
pub mod hkMeshSection_;
pub use hkMeshSection_::*;
pub mod hkMeshSectionCinfo_;
pub use hkMeshSectionCinfo_::*;
pub mod hkMeshShape_;
pub use hkMeshShape_::*;
pub mod hkMeshTexture_;
pub use hkMeshTexture_::*;
pub mod hkMeshVertexBuffer_;
pub use hkMeshVertexBuffer_::*;
pub mod hkModelerNodeTypeAttribute_;
pub use hkModelerNodeTypeAttribute_::*;
pub mod hkMonitorStreamColorTable_;
pub use hkMonitorStreamColorTable_::*;
pub mod hkMonitorStreamColorTableColorPair_;
pub use hkMonitorStreamColorTableColorPair_::*;
pub mod hkMonitorStreamFrameInfo_;
pub use hkMonitorStreamFrameInfo_::*;
pub mod hkMonitorStreamStringMap_;
pub use hkMonitorStreamStringMap_::*;
pub mod hkMonitorStreamStringMapStringMap_;
pub use hkMonitorStreamStringMapStringMap_::*;
pub mod hkMoppBvTreeShapeBase_;
pub use hkMoppBvTreeShapeBase_::*;
pub mod hkMotionState_;
pub use hkMotionState_::*;
pub mod hkMultipleVertexBuffer_;
pub use hkMultipleVertexBuffer_::*;
pub mod hkMultipleVertexBufferElementInfo_;
pub use hkMultipleVertexBufferElementInfo_::*;
pub mod hkMultipleVertexBufferLockedElement_;
pub use hkMultipleVertexBufferLockedElement_::*;
pub mod hkMultipleVertexBufferVertexBufferInfo_;
pub use hkMultipleVertexBufferVertexBufferInfo_::*;
pub mod hkMultiThreadCheck_;
pub use hkMultiThreadCheck_::*;
pub mod hkp2dAngConstraintAtom_;
pub use hkp2dAngConstraintAtom_::*;
pub mod hkpAabbPhantom_;
pub use hkpAabbPhantom_::*;
pub mod hkPackedVector3_;
pub use hkPackedVector3_::*;
pub mod hkPackfileHeader_;
pub use hkPackfileHeader_::*;
pub mod hkPackfileSectionHeader_;
pub use hkPackfileSectionHeader_::*;
pub mod hkpAction_;
pub use hkpAction_::*;
pub mod hkpAgent1nSector_;
pub use hkpAgent1nSector_::*;
pub mod hkpAngConstraintAtom_;
pub use hkpAngConstraintAtom_::*;
pub mod hkpAngFrictionConstraintAtom_;
pub use hkpAngFrictionConstraintAtom_::*;
pub mod hkpAngLimitConstraintAtom_;
pub use hkpAngLimitConstraintAtom_::*;
pub mod hkpAngMotorConstraintAtom_;
pub use hkpAngMotorConstraintAtom_::*;
pub mod hkpAngularDashpotAction_;
pub use hkpAngularDashpotAction_::*;
pub mod hkpArrayAction_;
pub use hkpArrayAction_::*;
pub mod hkpBallAndSocketConstraintData_;
pub use hkpBallAndSocketConstraintData_::*;
pub mod hkpBallAndSocketConstraintDataAtoms_;
pub use hkpBallAndSocketConstraintDataAtoms_::*;
pub mod hkpBallGun_;
pub use hkpBallGun_::*;
pub mod hkpBallSocketChainData_;
pub use hkpBallSocketChainData_::*;
pub mod hkpBallSocketChainDataConstraintInfo_;
pub use hkpBallSocketChainDataConstraintInfo_::*;
pub mod hkpBallSocketConstraintAtom_;
pub use hkpBallSocketConstraintAtom_::*;
pub mod hkpBinaryAction_;
pub use hkpBinaryAction_::*;
pub mod hkpBoxMotion_;
pub use hkpBoxMotion_::*;
pub mod hkpBoxShape_;
pub use hkpBoxShape_::*;
pub mod hkpBreakableBody_;
pub use hkpBreakableBody_::*;
pub mod hkpBreakableConstraintData_;
pub use hkpBreakableConstraintData_::*;
pub mod hkpBridgeAtoms_;
pub use hkpBridgeAtoms_::*;
pub mod hkpBridgeConstraintAtom_;
pub use hkpBridgeConstraintAtom_::*;
pub mod hkpBroadPhaseHandle_;
pub use hkpBroadPhaseHandle_::*;
pub mod hkpBvShape_;
pub use hkpBvShape_::*;
pub mod hkpBvTreeShape_;
pub use hkpBvTreeShape_::*;
pub mod hkpCachingShapePhantom_;
pub use hkpCachingShapePhantom_::*;
pub mod hkpCallbackConstraintMotor_;
pub use hkpCallbackConstraintMotor_::*;
pub mod hkpCapsuleShape_;
pub use hkpCapsuleShape_::*;
pub mod hkpCdBody_;
pub use hkpCdBody_::*;
pub mod hkpCenterOfMassChangerModifierConstraintAtom_;
pub use hkpCenterOfMassChangerModifierConstraintAtom_::*;
pub mod hkpCharacterControllerCinfo_;
pub use hkpCharacterControllerCinfo_::*;
pub mod hkpCharacterMotion_;
pub use hkpCharacterMotion_::*;
pub mod hkpCharacterProxyCinfo_;
pub use hkpCharacterProxyCinfo_::*;
pub mod hkpCharacterRigidBodyCinfo_;
pub use hkpCharacterRigidBodyCinfo_::*;
pub mod hkpCogWheelConstraintAtom_;
pub use hkpCogWheelConstraintAtom_::*;
pub mod hkpCogWheelConstraintData_;
pub use hkpCogWheelConstraintData_::*;
pub mod hkpCogWheelConstraintDataAtoms_;
pub use hkpCogWheelConstraintDataAtoms_::*;
pub mod hkpCollidable_;
pub use hkpCollidable_::*;
pub mod hkpCollidableBoundingVolumeData_;
pub use hkpCollidableBoundingVolumeData_::*;
pub mod hkpCollidableCollidableFilter_;
pub use hkpCollidableCollidableFilter_::*;
pub mod hkpCollisionFilter_;
pub use hkpCollisionFilter_::*;
pub mod hkpCollisionFilterList_;
pub use hkpCollisionFilterList_::*;
pub mod hkpCompressedMeshShape_;
pub use hkpCompressedMeshShape_::*;
pub mod hkpCompressedMeshShapeBigTriangle_;
pub use hkpCompressedMeshShapeBigTriangle_::*;
pub mod hkpCompressedMeshShapeChunk_;
pub use hkpCompressedMeshShapeChunk_::*;
pub mod hkpCompressedMeshShapeConvexPiece_;
pub use hkpCompressedMeshShapeConvexPiece_::*;
pub mod hkpCompressedSampledHeightFieldShape_;
pub use hkpCompressedSampledHeightFieldShape_::*;
pub mod hkpConeLimitConstraintAtom_;
pub use hkpConeLimitConstraintAtom_::*;
pub mod hkpConstrainedSystemFilter_;
pub use hkpConstrainedSystemFilter_::*;
pub mod hkpConstraintAtom_;
pub use hkpConstraintAtom_::*;
pub mod hkpConstraintChainData_;
pub use hkpConstraintChainData_::*;
pub mod hkpConstraintChainInstance_;
pub use hkpConstraintChainInstance_::*;
pub mod hkpConstraintChainInstanceAction_;
pub use hkpConstraintChainInstanceAction_::*;
pub mod hkpConstraintCollisionFilter_;
pub use hkpConstraintCollisionFilter_::*;
pub mod hkpConstraintData_;
pub use hkpConstraintData_::*;
pub mod hkpConstraintInstance_;
pub use hkpConstraintInstance_::*;
pub mod hkpConstraintInstanceSmallArraySerializeOverrideType_;
pub use hkpConstraintInstanceSmallArraySerializeOverrideType_::*;
pub mod hkpConstraintMotor_;
pub use hkpConstraintMotor_::*;
pub mod hkpConvexListFilter_;
pub use hkpConvexListFilter_::*;
pub mod hkpConvexListShape_;
pub use hkpConvexListShape_::*;
pub mod hkpConvexPieceMeshShape_;
pub use hkpConvexPieceMeshShape_::*;
pub mod hkpConvexPieceStreamData_;
pub use hkpConvexPieceStreamData_::*;
pub mod hkpConvexShape_;
pub use hkpConvexShape_::*;
pub mod hkpConvexTransformShape_;
pub use hkpConvexTransformShape_::*;
pub mod hkpConvexTransformShapeBase_;
pub use hkpConvexTransformShapeBase_::*;
pub mod hkpConvexTranslateShape_;
pub use hkpConvexTranslateShape_::*;
pub mod hkpConvexVerticesConnectivity_;
pub use hkpConvexVerticesConnectivity_::*;
pub mod hkpConvexVerticesShape_;
pub use hkpConvexVerticesShape_::*;
pub mod hkpConvexVerticesShapeFourVectors_;
pub use hkpConvexVerticesShapeFourVectors_::*;
pub mod hkpCylinderShape_;
pub use hkpCylinderShape_::*;
pub mod hkpDashpotAction_;
pub use hkpDashpotAction_::*;
pub mod hkpDefaultConvexListFilter_;
pub use hkpDefaultConvexListFilter_::*;
pub mod hkpDefaultWorldMemoryWatchDog_;
pub use hkpDefaultWorldMemoryWatchDog_::*;
pub mod hkpDisableEntityCollisionFilter_;
pub use hkpDisableEntityCollisionFilter_::*;
pub mod hkpDisplayBindingData_;
pub use hkpDisplayBindingData_::*;
pub mod hkpDisplayBindingDataPhysicsSystem_;
pub use hkpDisplayBindingDataPhysicsSystem_::*;
pub mod hkpDisplayBindingDataRigidBody_;
pub use hkpDisplayBindingDataRigidBody_::*;
pub mod hkpEntity_;
pub use hkpEntity_::*;
pub mod hkpEntityExtendedListeners_;
pub use hkpEntityExtendedListeners_::*;
pub mod hkpEntitySmallArraySerializeOverrideType_;
pub use hkpEntitySmallArraySerializeOverrideType_::*;
pub mod hkpEntitySpuCollisionCallback_;
pub use hkpEntitySpuCollisionCallback_::*;
pub mod hkpExtendedMeshShape_;
pub use hkpExtendedMeshShape_::*;
pub mod hkpExtendedMeshShapeShapesSubpart_;
pub use hkpExtendedMeshShapeShapesSubpart_::*;
pub mod hkpExtendedMeshShapeSubpart_;
pub use hkpExtendedMeshShapeSubpart_::*;
pub mod hkpExtendedMeshShapeTrianglesSubpart_;
pub use hkpExtendedMeshShapeTrianglesSubpart_::*;
pub mod hkpFastMeshShape_;
pub use hkpFastMeshShape_::*;
pub mod hkpFirstPersonGun_;
pub use hkpFirstPersonGun_::*;
pub mod hkpFixedRigidMotion_;
pub use hkpFixedRigidMotion_::*;
pub mod hkpGenericConstraintData_;
pub use hkpGenericConstraintData_::*;
pub mod hkpGenericConstraintDataScheme_;
pub use hkpGenericConstraintDataScheme_::*;
pub mod hkpGenericConstraintDataSchemeConstraintInfo_;
pub use hkpGenericConstraintDataSchemeConstraintInfo_::*;
pub mod hkpGravityGun_;
pub use hkpGravityGun_::*;
pub mod hkpGroupCollisionFilter_;
pub use hkpGroupCollisionFilter_::*;
pub mod hkpGroupFilter_;
pub use hkpGroupFilter_::*;
pub mod hkpHeightFieldShape_;
pub use hkpHeightFieldShape_::*;
pub mod hkpHingeConstraintData_;
pub use hkpHingeConstraintData_::*;
pub mod hkpHingeConstraintDataAtoms_;
pub use hkpHingeConstraintDataAtoms_::*;
pub mod hkpHingeLimitsData_;
pub use hkpHingeLimitsData_::*;
pub mod hkpHingeLimitsDataAtoms_;
pub use hkpHingeLimitsDataAtoms_::*;
pub mod hkpIgnoreModifierConstraintAtom_;
pub use hkpIgnoreModifierConstraintAtom_::*;
pub mod hkpKeyframedRigidMotion_;
pub use hkpKeyframedRigidMotion_::*;
pub mod hkpLimitedForceConstraintMotor_;
pub use hkpLimitedForceConstraintMotor_::*;
pub mod hkpLimitedHingeConstraintData_;
pub use hkpLimitedHingeConstraintData_::*;
pub mod hkpLimitedHingeConstraintDataAtoms_;
pub use hkpLimitedHingeConstraintDataAtoms_::*;
pub mod hkpLinConstraintAtom_;
pub use hkpLinConstraintAtom_::*;
pub mod hkpLinearParametricCurve_;
pub use hkpLinearParametricCurve_::*;
pub mod hkpLinFrictionConstraintAtom_;
pub use hkpLinFrictionConstraintAtom_::*;
pub mod hkpLinkedCollidable_;
pub use hkpLinkedCollidable_::*;
pub mod hkpLinLimitConstraintAtom_;
pub use hkpLinLimitConstraintAtom_::*;
pub mod hkpLinMotorConstraintAtom_;
pub use hkpLinMotorConstraintAtom_::*;
pub mod hkpLinSoftConstraintAtom_;
pub use hkpLinSoftConstraintAtom_::*;
pub mod hkpListShape_;
pub use hkpListShape_::*;
pub mod hkpListShapeChildInfo_;
pub use hkpListShapeChildInfo_::*;
pub mod hkpMalleableConstraintData_;
pub use hkpMalleableConstraintData_::*;
pub mod hkpMassChangerModifierConstraintAtom_;
pub use hkpMassChangerModifierConstraintAtom_::*;
pub mod hkpMassProperties_;
pub use hkpMassProperties_::*;
pub mod hkpMaterial_;
pub use hkpMaterial_::*;
pub mod hkpMaxSizeMotion_;
pub use hkpMaxSizeMotion_::*;
pub mod hkpMeshMaterial_;
pub use hkpMeshMaterial_::*;
pub mod hkpMeshShape_;
pub use hkpMeshShape_::*;
pub mod hkpMeshShapeSubpart_;
pub use hkpMeshShapeSubpart_::*;
pub mod hkpModifierConstraintAtom_;
pub use hkpModifierConstraintAtom_::*;
pub mod hkpMoppBvTreeShape_;
pub use hkpMoppBvTreeShape_::*;
pub mod hkpMoppCode_;
pub use hkpMoppCode_::*;
pub mod hkpMoppCodeCodeInfo_;
pub use hkpMoppCodeCodeInfo_::*;
pub mod hkpMoppCodeReindexedTerminal_;
pub use hkpMoppCodeReindexedTerminal_::*;
pub mod hkpMotion_;
pub use hkpMotion_::*;
pub mod hkpMotorAction_;
pub use hkpMotorAction_::*;
pub mod hkpMountedBallGun_;
pub use hkpMountedBallGun_::*;
pub mod hkpMouseSpringAction_;
pub use hkpMouseSpringAction_::*;
pub mod hkpMovingSurfaceModifierConstraintAtom_;
pub use hkpMovingSurfaceModifierConstraintAtom_::*;
pub mod hkpMultiRayShape_;
pub use hkpMultiRayShape_::*;
pub mod hkpMultiRayShapeRay_;
pub use hkpMultiRayShapeRay_::*;
pub mod hkpMultiSphereShape_;
pub use hkpMultiSphereShape_::*;
pub mod hkpMultithreadedVehicleManager_;
pub use hkpMultithreadedVehicleManager_::*;
pub mod hkpNamedMeshMaterial_;
pub use hkpNamedMeshMaterial_::*;
pub mod hkpNullCollisionFilter_;
pub use hkpNullCollisionFilter_::*;
pub mod hkPostFinishAttribute_;
pub use hkPostFinishAttribute_::*;
pub mod hkpOverwritePivotConstraintAtom_;
pub use hkpOverwritePivotConstraintAtom_::*;
pub mod hkpPairCollisionFilter_;
pub use hkpPairCollisionFilter_::*;
pub mod hkpPairCollisionFilterMapPairFilterKeyOverrideType_;
pub use hkpPairCollisionFilterMapPairFilterKeyOverrideType_::*;
pub mod hkpParametricCurve_;
pub use hkpParametricCurve_::*;
pub mod hkpPhantom_;
pub use hkpPhantom_::*;
pub mod hkpPhantomCallbackShape_;
pub use hkpPhantomCallbackShape_::*;
pub mod hkpPhysicsData_;
pub use hkpPhysicsData_::*;
pub mod hkpPhysicsSystem_;
pub use hkpPhysicsSystem_::*;
pub mod hkpPhysicsSystemWithContacts_;
pub use hkpPhysicsSystemWithContacts_::*;
pub mod hkpPlaneShape_;
pub use hkpPlaneShape_::*;
pub mod hkpPointToPathConstraintData_;
pub use hkpPointToPathConstraintData_::*;
pub mod hkpPointToPlaneConstraintData_;
pub use hkpPointToPlaneConstraintData_::*;
pub mod hkpPointToPlaneConstraintDataAtoms_;
pub use hkpPointToPlaneConstraintDataAtoms_::*;
pub mod hkpPositionConstraintMotor_;
pub use hkpPositionConstraintMotor_::*;
pub mod hkpPoweredChainData_;
pub use hkpPoweredChainData_::*;
pub mod hkpPoweredChainDataConstraintInfo_;
pub use hkpPoweredChainDataConstraintInfo_::*;
pub mod hkpPoweredChainMapper_;
pub use hkpPoweredChainMapper_::*;
pub mod hkpPoweredChainMapperLinkInfo_;
pub use hkpPoweredChainMapperLinkInfo_::*;
pub mod hkpPoweredChainMapperTarget_;
pub use hkpPoweredChainMapperTarget_::*;
pub mod hkpPrismaticConstraintData_;
pub use hkpPrismaticConstraintData_::*;
pub mod hkpPrismaticConstraintDataAtoms_;
pub use hkpPrismaticConstraintDataAtoms_::*;
pub mod hkpProjectileGun_;
pub use hkpProjectileGun_::*;
pub mod hkpProperty_;
pub use hkpProperty_::*;
pub mod hkpPropertyValue_;
pub use hkpPropertyValue_::*;
pub mod hkpPulleyConstraintAtom_;
pub use hkpPulleyConstraintAtom_::*;
pub mod hkpPulleyConstraintData_;
pub use hkpPulleyConstraintData_::*;
pub mod hkpPulleyConstraintDataAtoms_;
pub use hkpPulleyConstraintDataAtoms_::*;
pub mod hkpRackAndPinionConstraintAtom_;
pub use hkpRackAndPinionConstraintAtom_::*;
pub mod hkpRackAndPinionConstraintData_;
pub use hkpRackAndPinionConstraintData_::*;
pub mod hkpRackAndPinionConstraintDataAtoms_;
pub use hkpRackAndPinionConstraintDataAtoms_::*;
pub mod hkpRagdollConstraintData_;
pub use hkpRagdollConstraintData_::*;
pub mod hkpRagdollConstraintDataAtoms_;
pub use hkpRagdollConstraintDataAtoms_::*;
pub mod hkpRagdollLimitsData_;
pub use hkpRagdollLimitsData_::*;
pub mod hkpRagdollLimitsDataAtoms_;
pub use hkpRagdollLimitsDataAtoms_::*;
pub mod hkpRagdollMotorConstraintAtom_;
pub use hkpRagdollMotorConstraintAtom_::*;
pub mod hkpRayCollidableFilter_;
pub use hkpRayCollidableFilter_::*;
pub mod hkpRayShapeCollectionFilter_;
pub use hkpRayShapeCollectionFilter_::*;
pub mod hkpRejectChassisListener_;
pub use hkpRejectChassisListener_::*;
pub mod hkpRemoveTerminalsMoppModifier_;
pub use hkpRemoveTerminalsMoppModifier_::*;
pub mod hkpReorientAction_;
pub use hkpReorientAction_::*;
pub mod hkpRigidBody_;
pub use hkpRigidBody_::*;
pub mod hkpRotationalConstraintData_;
pub use hkpRotationalConstraintData_::*;
pub mod hkpRotationalConstraintDataAtoms_;
pub use hkpRotationalConstraintDataAtoms_::*;
pub mod hkpSampledHeightFieldShape_;
pub use hkpSampledHeightFieldShape_::*;
pub mod hkpSerializedAgentNnEntry_;
pub use hkpSerializedAgentNnEntry_::*;
pub mod hkpSerializedDisplayMarker_;
pub use hkpSerializedDisplayMarker_::*;
pub mod hkpSerializedDisplayMarkerList_;
pub use hkpSerializedDisplayMarkerList_::*;
pub mod hkpSerializedDisplayRbTransforms_;
pub use hkpSerializedDisplayRbTransforms_::*;
pub mod hkpSerializedDisplayRbTransformsDisplayTransformPair_;
pub use hkpSerializedDisplayRbTransformsDisplayTransformPair_::*;
pub mod hkpSerializedSubTrack1nInfo_;
pub use hkpSerializedSubTrack1nInfo_::*;
pub mod hkpSerializedTrack1nInfo_;
pub use hkpSerializedTrack1nInfo_::*;
pub mod hkpSetLocalRotationsConstraintAtom_;
pub use hkpSetLocalRotationsConstraintAtom_::*;
pub mod hkpSetLocalTransformsConstraintAtom_;
pub use hkpSetLocalTransformsConstraintAtom_::*;
pub mod hkpSetLocalTranslationsConstraintAtom_;
pub use hkpSetLocalTranslationsConstraintAtom_::*;
pub mod hkpSetupStabilizationAtom_;
pub use hkpSetupStabilizationAtom_::*;
pub mod hkpShape_;
pub use hkpShape_::*;
pub mod hkpShapeCollection_;
pub use hkpShapeCollection_::*;
pub mod hkpShapeCollectionFilter_;
pub use hkpShapeCollectionFilter_::*;
pub mod hkpShapeContainer_;
pub use hkpShapeContainer_::*;
pub mod hkpShapeInfo_;
pub use hkpShapeInfo_::*;
pub mod hkpShapeModifier_;
pub use hkpShapeModifier_::*;
pub mod hkpShapePhantom_;
pub use hkpShapePhantom_::*;
pub mod hkpSimpleContactConstraintAtom_;
pub use hkpSimpleContactConstraintAtom_::*;
pub mod hkpSimpleContactConstraintDataInfo_;
pub use hkpSimpleContactConstraintDataInfo_::*;
pub mod hkpSimpleMeshShape_;
pub use hkpSimpleMeshShape_::*;
pub mod hkpSimpleMeshShapeTriangle_;
pub use hkpSimpleMeshShapeTriangle_::*;
pub mod hkpSimpleShapePhantom_;
pub use hkpSimpleShapePhantom_::*;
pub mod hkpSimpleShapePhantomCollisionDetail_;
pub use hkpSimpleShapePhantomCollisionDetail_::*;
pub mod hkpSimulation_;
pub use hkpSimulation_::*;
pub mod hkpSingleShapeContainer_;
pub use hkpSingleShapeContainer_::*;
pub mod hkpSoftContactModifierConstraintAtom_;
pub use hkpSoftContactModifierConstraintAtom_::*;
pub mod hkpSphereMotion_;
pub use hkpSphereMotion_::*;
pub mod hkpSphereRepShape_;
pub use hkpSphereRepShape_::*;
pub mod hkpSphereShape_;
pub use hkpSphereShape_::*;
pub mod hkpSpringAction_;
pub use hkpSpringAction_::*;
pub mod hkpSpringDamperConstraintMotor_;
pub use hkpSpringDamperConstraintMotor_::*;
pub mod hkpStiffSpringChainData_;
pub use hkpStiffSpringChainData_::*;
pub mod hkpStiffSpringChainDataConstraintInfo_;
pub use hkpStiffSpringChainDataConstraintInfo_::*;
pub mod hkpStiffSpringConstraintAtom_;
pub use hkpStiffSpringConstraintAtom_::*;
pub mod hkpStiffSpringConstraintData_;
pub use hkpStiffSpringConstraintData_::*;
pub mod hkpStiffSpringConstraintDataAtoms_;
pub use hkpStiffSpringConstraintDataAtoms_::*;
pub mod hkpStorageExtendedMeshShape_;
pub use hkpStorageExtendedMeshShape_::*;
pub mod hkpStorageExtendedMeshShapeMaterial_;
pub use hkpStorageExtendedMeshShapeMaterial_::*;
pub mod hkpStorageExtendedMeshShapeMeshSubpartStorage_;
pub use hkpStorageExtendedMeshShapeMeshSubpartStorage_::*;
pub mod hkpStorageExtendedMeshShapeShapeSubpartStorage_;
pub use hkpStorageExtendedMeshShapeShapeSubpartStorage_::*;
pub mod hkpStorageMeshShape_;
pub use hkpStorageMeshShape_::*;
pub mod hkpStorageMeshShapeSubpartStorage_;
pub use hkpStorageMeshShapeSubpartStorage_::*;
pub mod hkpStorageSampledHeightFieldShape_;
pub use hkpStorageSampledHeightFieldShape_::*;
pub mod hkpThinBoxMotion_;
pub use hkpThinBoxMotion_::*;
pub mod hkpTransformShape_;
pub use hkpTransformShape_::*;
pub mod hkpTriangleShape_;
pub use hkpTriangleShape_::*;
pub mod hkpTriggerVolume_;
pub use hkpTriggerVolume_::*;
pub mod hkpTriggerVolumeEventInfo_;
pub use hkpTriggerVolumeEventInfo_::*;
pub mod hkpTriSampledHeightFieldBvTreeShape_;
pub use hkpTriSampledHeightFieldBvTreeShape_::*;
pub mod hkpTriSampledHeightFieldCollection_;
pub use hkpTriSampledHeightFieldCollection_::*;
pub mod hkpTwistLimitConstraintAtom_;
pub use hkpTwistLimitConstraintAtom_::*;
pub mod hkpTypedBroadPhaseHandle_;
pub use hkpTypedBroadPhaseHandle_::*;
pub mod hkpTyremarkPoint_;
pub use hkpTyremarkPoint_::*;
pub mod hkpTyremarksInfo_;
pub use hkpTyremarksInfo_::*;
pub mod hkpTyremarksWheel_;
pub use hkpTyremarksWheel_::*;
pub mod hkpUnaryAction_;
pub use hkpUnaryAction_::*;
pub mod hkpVehicleAerodynamics_;
pub use hkpVehicleAerodynamics_::*;
pub mod hkpVehicleBrake_;
pub use hkpVehicleBrake_::*;
pub mod hkpVehicleCastBatchingManager_;
pub use hkpVehicleCastBatchingManager_::*;
pub mod hkpVehicleData_;
pub use hkpVehicleData_::*;
pub mod hkpVehicleDataWheelComponentParams_;
pub use hkpVehicleDataWheelComponentParams_::*;
pub mod hkpVehicleDefaultAerodynamics_;
pub use hkpVehicleDefaultAerodynamics_::*;
pub mod hkpVehicleDefaultAnalogDriverInput_;
pub use hkpVehicleDefaultAnalogDriverInput_::*;
pub mod hkpVehicleDefaultBrake_;
pub use hkpVehicleDefaultBrake_::*;
pub mod hkpVehicleDefaultBrakeWheelBrakingProperties_;
pub use hkpVehicleDefaultBrakeWheelBrakingProperties_::*;
pub mod hkpVehicleDefaultEngine_;
pub use hkpVehicleDefaultEngine_::*;
pub mod hkpVehicleDefaultSteering_;
pub use hkpVehicleDefaultSteering_::*;
pub mod hkpVehicleDefaultSuspension_;
pub use hkpVehicleDefaultSuspension_::*;
pub mod hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters_;
pub use hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters_::*;
pub mod hkpVehicleDefaultTransmission_;
pub use hkpVehicleDefaultTransmission_::*;
pub mod hkpVehicleDefaultVelocityDamper_;
pub use hkpVehicleDefaultVelocityDamper_::*;
pub mod hkpVehicleDriverInput_;
pub use hkpVehicleDriverInput_::*;
pub mod hkpVehicleDriverInputAnalogStatus_;
pub use hkpVehicleDriverInputAnalogStatus_::*;
pub mod hkpVehicleDriverInputStatus_;
pub use hkpVehicleDriverInputStatus_::*;
pub mod hkpVehicleEngine_;
pub use hkpVehicleEngine_::*;
pub mod hkpVehicleFrictionDescription_;
pub use hkpVehicleFrictionDescription_::*;
pub mod hkpVehicleFrictionDescriptionAxisDescription_;
pub use hkpVehicleFrictionDescriptionAxisDescription_::*;
pub mod hkpVehicleFrictionStatus_;
pub use hkpVehicleFrictionStatus_::*;
pub mod hkpVehicleFrictionStatusAxisStatus_;
pub use hkpVehicleFrictionStatusAxisStatus_::*;
pub mod hkpVehicleInstance_;
pub use hkpVehicleInstance_::*;
pub mod hkpVehicleInstanceWheelInfo_;
pub use hkpVehicleInstanceWheelInfo_::*;
pub mod hkpVehicleLinearCastBatchingManager_;
pub use hkpVehicleLinearCastBatchingManager_::*;
pub mod hkpVehicleLinearCastWheelCollide_;
pub use hkpVehicleLinearCastWheelCollide_::*;
pub mod hkpVehicleLinearCastWheelCollideWheelState_;
pub use hkpVehicleLinearCastWheelCollideWheelState_::*;
pub mod hkpVehicleManager_;
pub use hkpVehicleManager_::*;
pub mod hkpVehicleRayCastBatchingManager_;
pub use hkpVehicleRayCastBatchingManager_::*;
pub mod hkpVehicleRayCastWheelCollide_;
pub use hkpVehicleRayCastWheelCollide_::*;
pub mod hkpVehicleSteering_;
pub use hkpVehicleSteering_::*;
pub mod hkpVehicleSuspension_;
pub use hkpVehicleSuspension_::*;
pub mod hkpVehicleSuspensionSuspensionWheelParameters_;
pub use hkpVehicleSuspensionSuspensionWheelParameters_::*;
pub mod hkpVehicleTransmission_;
pub use hkpVehicleTransmission_::*;
pub mod hkpVehicleVelocityDamper_;
pub use hkpVehicleVelocityDamper_::*;
pub mod hkpVehicleWheelCollide_;
pub use hkpVehicleWheelCollide_::*;
pub mod hkpVelocityConstraintMotor_;
pub use hkpVelocityConstraintMotor_::*;
pub mod hkpViscousSurfaceModifierConstraintAtom_;
pub use hkpViscousSurfaceModifierConstraintAtom_::*;
pub mod hkpWeldingUtility_;
pub use hkpWeldingUtility_::*;
pub mod hkpWheelConstraintData_;
pub use hkpWheelConstraintData_::*;
pub mod hkpWheelConstraintDataAtoms_;
pub use hkpWheelConstraintDataAtoms_::*;
pub mod hkpWorld_;
pub use hkpWorld_::*;
pub mod hkpWorldCinfo_;
pub use hkpWorldCinfo_::*;
pub mod hkpWorldObject_;
pub use hkpWorldObject_::*;
pub mod hkQTransform_;
pub use hkQTransform_::*;
pub mod hkRangeInt32Attribute_;
pub use hkRangeInt32Attribute_::*;
pub mod hkRangeRealAttribute_;
pub use hkRangeRealAttribute_::*;
pub mod hkReferencedObject_;
pub use hkReferencedObject_::*;
pub mod hkReflectedFileAttribute_;
pub use hkReflectedFileAttribute_::*;
pub mod hkResourceBase_;
pub use hkResourceBase_::*;
pub mod hkResourceContainer_;
pub use hkResourceContainer_::*;
pub mod hkResourceHandle_;
pub use hkResourceHandle_::*;
pub mod hkRootLevelContainer_;
pub use hkRootLevelContainer_::*;
pub mod hkRootLevelContainerNamedVariant_;
pub use hkRootLevelContainerNamedVariant_::*;
pub mod hkSemanticsAttribute_;
pub use hkSemanticsAttribute_::*;
pub mod hkSimpleLocalFrame_;
pub use hkSimpleLocalFrame_::*;
pub mod hkSphere_;
pub use hkSphere_::*;
pub mod hkSweptTransform_;
pub use hkSweptTransform_::*;
pub mod hkTraceStreamTitle_;
pub use hkTraceStreamTitle_::*;
pub mod hkTrackerSerializableScanSnapshot_;
pub use hkTrackerSerializableScanSnapshot_::*;
pub mod hkTrackerSerializableScanSnapshotAllocation_;
pub use hkTrackerSerializableScanSnapshotAllocation_::*;
pub mod hkTrackerSerializableScanSnapshotBlock_;
pub use hkTrackerSerializableScanSnapshotBlock_::*;
pub mod hkUiAttribute_;
pub use hkUiAttribute_::*;
pub mod hkVertexFormat_;
pub use hkVertexFormat_::*;
pub mod hkVertexFormatElement_;
pub use hkVertexFormatElement_::*;
pub mod hkWorldMemoryAvailableWatchDog_;
pub use hkWorldMemoryAvailableWatchDog_::*;
pub mod hkxAnimatedFloat_;
pub use hkxAnimatedFloat_::*;
pub mod hkxAnimatedMatrix_;
pub use hkxAnimatedMatrix_::*;
pub mod hkxAnimatedQuaternion_;
pub use hkxAnimatedQuaternion_::*;
pub mod hkxAnimatedVector_;
pub use hkxAnimatedVector_::*;
pub mod hkxAttribute_;
pub use hkxAttribute_::*;
pub mod hkxAttributeGroup_;
pub use hkxAttributeGroup_::*;
pub mod hkxAttributeHolder_;
pub use hkxAttributeHolder_::*;
pub mod hkxCamera_;
pub use hkxCamera_::*;
pub mod hkxEdgeSelectionChannel_;
pub use hkxEdgeSelectionChannel_::*;
pub mod hkxEnum_;
pub use hkxEnum_::*;
pub mod hkxEnumItem_;
pub use hkxEnumItem_::*;
pub mod hkxEnvironment_;
pub use hkxEnvironment_::*;
pub mod hkxEnvironmentVariable_;
pub use hkxEnvironmentVariable_::*;
pub mod hkxIndexBuffer_;
pub use hkxIndexBuffer_::*;
pub mod hkxLight_;
pub use hkxLight_::*;
pub mod hkxMaterial_;
pub use hkxMaterial_::*;
pub mod hkxMaterialEffect_;
pub use hkxMaterialEffect_::*;
pub mod hkxMaterialProperty_;
pub use hkxMaterialProperty_::*;
pub mod hkxMaterialShader_;
pub use hkxMaterialShader_::*;
pub mod hkxMaterialShaderSet_;
pub use hkxMaterialShaderSet_::*;
pub mod hkxMaterialTextureStage_;
pub use hkxMaterialTextureStage_::*;
pub mod hkxMesh_;
pub use hkxMesh_::*;
pub mod hkxMeshSection_;
pub use hkxMeshSection_::*;
pub mod hkxMeshUserChannelInfo_;
pub use hkxMeshUserChannelInfo_::*;
pub mod hkxNode_;
pub use hkxNode_::*;
pub mod hkxNodeAnnotationData_;
pub use hkxNodeAnnotationData_::*;
pub mod hkxNodeSelectionSet_;
pub use hkxNodeSelectionSet_::*;
pub mod hkxScene_;
pub use hkxScene_::*;
pub mod hkxSkinBinding_;
pub use hkxSkinBinding_::*;
pub mod hkxSparselyAnimatedBool_;
pub use hkxSparselyAnimatedBool_::*;
pub mod hkxSparselyAnimatedEnum_;
pub use hkxSparselyAnimatedEnum_::*;
pub mod hkxSparselyAnimatedInt_;
pub use hkxSparselyAnimatedInt_::*;
pub mod hkxSparselyAnimatedString_;
pub use hkxSparselyAnimatedString_::*;
pub mod hkxTextureFile_;
pub use hkxTextureFile_::*;
pub mod hkxTextureInplace_;
pub use hkxTextureInplace_::*;
pub mod hkxTriangleSelectionChannel_;
pub use hkxTriangleSelectionChannel_::*;
pub mod hkxVertexBuffer_;
pub use hkxVertexBuffer_::*;
pub mod hkxVertexBufferVertexData_;
pub use hkxVertexBufferVertexData_::*;
pub mod hkxVertexDescription_;
pub use hkxVertexDescription_::*;
pub mod hkxVertexDescriptionElementDecl_;
pub use hkxVertexDescriptionElementDecl_::*;
pub mod hkxVertexFloatDataChannel_;
pub use hkxVertexFloatDataChannel_::*;
pub mod hkxVertexIntDataChannel_;
pub use hkxVertexIntDataChannel_::*;
pub mod hkxVertexSelectionChannel_;
pub use hkxVertexSelectionChannel_::*;
pub mod hkxVertexVectorDataChannel_;
pub use hkxVertexVectorDataChannel_::*;
use havok_serde as _serde;
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Classes<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    BGSGamebryoSequenceGenerator(Box<BGSGamebryoSequenceGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSBoneSwitchGenerator(Box<BSBoneSwitchGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSBoneSwitchGeneratorBoneData(Box<BSBoneSwitchGeneratorBoneData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSComputeAddBoneAnimModifier(Box<BSComputeAddBoneAnimModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSCyclicBlendTransitionGenerator(Box<BSCyclicBlendTransitionGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSDecomposeVectorModifier(Box<BSDecomposeVectorModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSDirectAtModifier(Box<BSDirectAtModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSDistTriggerModifier(Box<BSDistTriggerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSEventEveryNEventsModifier(Box<BSEventEveryNEventsModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSEventOnDeactivateModifier(Box<BSEventOnDeactivateModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSEventOnFalseToTrueModifier(Box<BSEventOnFalseToTrueModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSGetTimeStepModifier(Box<BSGetTimeStepModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSInterpValueModifier(Box<BSInterpValueModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSIsActiveModifier(Box<BSIsActiveModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSIStateManagerModifier(Box<BSIStateManagerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSIStateManagerModifierBSiStateData(Box<BSIStateManagerModifierBSiStateData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSIStateManagerModifierBSIStateManagerStateListener(
        Box<BSIStateManagerModifierBSIStateManagerStateListener<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSiStateTaggingGenerator(Box<BSiStateTaggingGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSLimbIKModifier(Box<BSLimbIKModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSLookAtModifier(Box<BSLookAtModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSLookAtModifierBoneData(Box<BSLookAtModifierBoneData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSModifyOnceModifier(Box<BSModifyOnceModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSOffsetAnimationGenerator(Box<BSOffsetAnimationGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSPassByTargetTriggerModifier(Box<BSPassByTargetTriggerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSRagdollContactListenerModifier(Box<BSRagdollContactListenerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSSpeedSamplerModifier(Box<BSSpeedSamplerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSSynchronizedClipGenerator(Box<BSSynchronizedClipGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSTimerModifier(Box<BSTimerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    BSTweenerModifier(Box<BSTweenerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkAabb(Box<hkAabb<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkAabbHalf(Box<hkAabbHalf<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkAabbUint32(Box<hkAabbUint32<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnimatedReferenceFrame(Box<hkaAnimatedReferenceFrame<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnimation(Box<hkaAnimation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnimationBinding(Box<hkaAnimationBinding<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnimationContainer(Box<hkaAnimationContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnimationPreviewColorContainer(Box<hkaAnimationPreviewColorContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnnotationTrack(Box<hkaAnnotationTrack<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaAnnotationTrackAnnotation(Box<hkaAnnotationTrackAnnotation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaBone(Box<hkaBone<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaBoneAttachment(Box<hkaBoneAttachment<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaDefaultAnimatedReferenceFrame(Box<hkaDefaultAnimatedReferenceFrame<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaDeltaCompressedAnimation(Box<hkaDeltaCompressedAnimation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaDeltaCompressedAnimationQuantizationFormat(
        Box<hkaDeltaCompressedAnimationQuantizationFormat<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaFootstepAnalysisInfo(Box<hkaFootstepAnalysisInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaFootstepAnalysisInfoContainer(Box<hkaFootstepAnalysisInfoContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaInterleavedUncompressedAnimation(Box<hkaInterleavedUncompressedAnimation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaKeyFrameHierarchyUtility(Box<hkaKeyFrameHierarchyUtility<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaKeyFrameHierarchyUtilityControlData(
        Box<hkaKeyFrameHierarchyUtilityControlData<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkAlignSceneToNodeOptions(Box<hkAlignSceneToNodeOptions<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaMeshBinding(Box<hkaMeshBinding<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaMeshBindingMapping(Box<hkaMeshBindingMapping<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaQuantizedAnimation(Box<hkaQuantizedAnimation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaQuantizedAnimationTrackCompressionParams(
        Box<hkaQuantizedAnimationTrackCompressionParams<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaRagdollInstance(Box<hkaRagdollInstance<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkArrayTypeAttribute(Box<hkArrayTypeAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSkeleton(Box<hkaSkeleton<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSkeletonLocalFrameOnBone(Box<hkaSkeletonLocalFrameOnBone<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSkeletonMapper(Box<hkaSkeletonMapper<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSkeletonMapperData(Box<hkaSkeletonMapperData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSkeletonMapperDataChainMapping(Box<hkaSkeletonMapperDataChainMapping<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSkeletonMapperDataSimpleMapping(Box<hkaSkeletonMapperDataSimpleMapping<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSplineCompressedAnimation(Box<hkaSplineCompressedAnimation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSplineCompressedAnimationAnimationCompressionParams(
        Box<hkaSplineCompressedAnimationAnimationCompressionParams<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaSplineCompressedAnimationTrackCompressionParams(
        Box<hkaSplineCompressedAnimationTrackCompressionParams<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaWaveletCompressedAnimation(Box<hkaWaveletCompressedAnimation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaWaveletCompressedAnimationCompressionParams(
        Box<hkaWaveletCompressedAnimationCompressionParams<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkaWaveletCompressedAnimationQuantizationFormat(
        Box<hkaWaveletCompressedAnimationQuantizationFormat<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkBaseObject(Box<hkBaseObject<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbAttachmentModifier(Box<hkbAttachmentModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbAttachmentSetup(Box<hkbAttachmentSetup<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbAttributeModifier(Box<hkbAttributeModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbAttributeModifierAssignment(Box<hkbAttributeModifierAssignment<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbAuxiliaryNodeInfo(Box<hkbAuxiliaryNodeInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorEventsInfo(Box<hkbBehaviorEventsInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorGraph(Box<hkbBehaviorGraph<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorGraphData(Box<hkbBehaviorGraphData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorGraphInternalState(Box<hkbBehaviorGraphInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorGraphInternalStateInfo(Box<hkbBehaviorGraphInternalStateInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorGraphStringData(Box<hkbBehaviorGraphStringData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorInfo(Box<hkbBehaviorInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorInfoIdToNamePair(Box<hkbBehaviorInfoIdToNamePair<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBehaviorReferenceGenerator(Box<hkbBehaviorReferenceGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBindable(Box<hkbBindable<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlendCurveUtils(Box<hkbBlendCurveUtils<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlenderGenerator(Box<hkbBlenderGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlenderGeneratorChild(Box<hkbBlenderGeneratorChild<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlenderGeneratorChildInternalState(
        Box<hkbBlenderGeneratorChildInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlenderGeneratorInternalState(Box<hkbBlenderGeneratorInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlendingTransitionEffect(Box<hkbBlendingTransitionEffect<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBlendingTransitionEffectInternalState(
        Box<hkbBlendingTransitionEffectInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBoneIndexArray(Box<hkbBoneIndexArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBoneWeightArray(Box<hkbBoneWeightArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBoolVariableSequencedData(Box<hkbBoolVariableSequencedData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbBoolVariableSequencedDataSample(Box<hkbBoolVariableSequencedDataSample<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCameraShakeEventPayload(Box<hkbCameraShakeEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacter(Box<hkbCharacter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterAddedInfo(Box<hkbCharacterAddedInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterControlCommand(Box<hkbCharacterControlCommand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterControllerControlData(Box<hkbCharacterControllerControlData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterControllerModifier(Box<hkbCharacterControllerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterControllerModifierInternalState(
        Box<hkbCharacterControllerModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterData(Box<hkbCharacterData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterDataCharacterControllerInfo(
        Box<hkbCharacterDataCharacterControllerInfo<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterInfo(Box<hkbCharacterInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterSetup(Box<hkbCharacterSetup<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterSkinInfo(Box<hkbCharacterSkinInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterSteppedInfo(Box<hkbCharacterSteppedInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCharacterStringData(Box<hkbCharacterStringData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbClientCharacterState(Box<hkbClientCharacterState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbClipGenerator(Box<hkbClipGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbClipGeneratorEcho(Box<hkbClipGeneratorEcho<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbClipGeneratorInternalState(Box<hkbClipGeneratorInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbClipTrigger(Box<hkbClipTrigger<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbClipTriggerArray(Box<hkbClipTriggerArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCombineTransformsModifier(Box<hkbCombineTransformsModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCombineTransformsModifierInternalState(
        Box<hkbCombineTransformsModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCompiledExpressionSet(Box<hkbCompiledExpressionSet<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCompiledExpressionSetToken(Box<hkbCompiledExpressionSetToken<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbComputeDirectionModifier(Box<hkbComputeDirectionModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbComputeDirectionModifierInternalState(
        Box<hkbComputeDirectionModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbComputeRotationFromAxisAngleModifier(
        Box<hkbComputeRotationFromAxisAngleModifier<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbComputeRotationFromAxisAngleModifierInternalState(
        Box<hkbComputeRotationFromAxisAngleModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbComputeRotationToTargetModifier(Box<hkbComputeRotationToTargetModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbComputeRotationToTargetModifierInternalState(
        Box<hkbComputeRotationToTargetModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbCondition(Box<hkbCondition<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbContext(Box<hkbContext<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDampingModifier(Box<hkbDampingModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDampingModifierInternalState(Box<hkbDampingModifierInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDefaultMessageLog(Box<hkbDefaultMessageLog<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDelayedModifier(Box<hkbDelayedModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDelayedModifierInternalState(Box<hkbDelayedModifierInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDetectCloseToGroundModifier(Box<hkbDetectCloseToGroundModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbDetectCloseToGroundModifierInternalState(
        Box<hkbDetectCloseToGroundModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEvaluateExpressionModifier(Box<hkbEvaluateExpressionModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEvaluateExpressionModifierInternalExpressionData(
        Box<hkbEvaluateExpressionModifierInternalExpressionData<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEvaluateExpressionModifierInternalState(
        Box<hkbEvaluateExpressionModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEvaluateHandleModifier(Box<hkbEvaluateHandleModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEvent(Box<hkbEvent<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventBase(Box<hkbEventBase<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventDrivenModifier(Box<hkbEventDrivenModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventDrivenModifierInternalState(Box<hkbEventDrivenModifierInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventInfo(Box<hkbEventInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventPayload(Box<hkbEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventPayloadList(Box<hkbEventPayloadList<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventProperty(Box<hkbEventProperty<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventRaisedInfo(Box<hkbEventRaisedInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventRangeData(Box<hkbEventRangeData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventRangeDataArray(Box<hkbEventRangeDataArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventSequencedData(Box<hkbEventSequencedData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventSequencedDataSequencedEvent(Box<hkbEventSequencedDataSequencedEvent<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventsFromRangeModifier(Box<hkbEventsFromRangeModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbEventsFromRangeModifierInternalState(
        Box<hkbEventsFromRangeModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbExpressionCondition(Box<hkbExpressionCondition<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbExpressionData(Box<hkbExpressionData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbExpressionDataArray(Box<hkbExpressionDataArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbExtractRagdollPoseModifier(Box<hkbExtractRagdollPoseModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkControlData(Box<hkbFootIkControlData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkControlsModifier(Box<hkbFootIkControlsModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkControlsModifierLeg(Box<hkbFootIkControlsModifierLeg<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkDriverInfo(Box<hkbFootIkDriverInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkDriverInfoLeg(Box<hkbFootIkDriverInfoLeg<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkGains(Box<hkbFootIkGains<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkModifier(Box<hkbFootIkModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkModifierInternalLegData(Box<hkbFootIkModifierInternalLegData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbFootIkModifierLeg(Box<hkbFootIkModifierLeg<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGenerator(Box<hkbGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGeneratorOutputListener(Box<hkbGeneratorOutputListener<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGeneratorSyncInfo(Box<hkbGeneratorSyncInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGeneratorSyncInfoSyncPoint(Box<hkbGeneratorSyncInfoSyncPoint<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGeneratorTransitionEffect(Box<hkbGeneratorTransitionEffect<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGeneratorTransitionEffectInternalState(
        Box<hkbGeneratorTransitionEffectInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGetHandleOnBoneModifier(Box<hkbGetHandleOnBoneModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGetUpModifier(Box<hkbGetUpModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGetUpModifierInternalState(Box<hkbGetUpModifierInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGetWorldFromModelModifier(Box<hkbGetWorldFromModelModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbGetWorldFromModelModifierInternalState(
        Box<hkbGetWorldFromModelModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkControlData(Box<hkbHandIkControlData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkControlsModifier(Box<hkbHandIkControlsModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkControlsModifierHand(Box<hkbHandIkControlsModifierHand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkDriverInfo(Box<hkbHandIkDriverInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkDriverInfoHand(Box<hkbHandIkDriverInfoHand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkModifier(Box<hkbHandIkModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandIkModifierHand(Box<hkbHandIkModifierHand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbHandle(Box<hkbHandle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbIntEventPayload(Box<hkbIntEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbIntVariableSequencedData(Box<hkbIntVariableSequencedData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbIntVariableSequencedDataSample(Box<hkbIntVariableSequencedDataSample<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkBitField(Box<hkBitField<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbKeyframeBonesModifier(Box<hkbKeyframeBonesModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbKeyframeBonesModifierKeyframeInfo(Box<hkbKeyframeBonesModifierKeyframeInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbLinkedSymbolInfo(Box<hkbLinkedSymbolInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbLookAtModifier(Box<hkbLookAtModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbLookAtModifierInternalState(Box<hkbLookAtModifierInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbManualSelectorGenerator(Box<hkbManualSelectorGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbManualSelectorGeneratorInternalState(
        Box<hkbManualSelectorGeneratorInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbMessageLog(Box<hkbMessageLog<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbMirroredSkeletonInfo(Box<hkbMirroredSkeletonInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbMirrorModifier(Box<hkbMirrorModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbModifier(Box<hkbModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbModifierGenerator(Box<hkbModifierGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbModifierList(Box<hkbModifierList<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbModifierWrapper(Box<hkbModifierWrapper<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbMoveCharacterModifier(Box<hkbMoveCharacterModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbMoveCharacterModifierInternalState(
        Box<hkbMoveCharacterModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbNamedEventPayload(Box<hkbNamedEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbNamedIntEventPayload(Box<hkbNamedIntEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbNamedRealEventPayload(Box<hkbNamedRealEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbNamedStringEventPayload(Box<hkbNamedStringEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbNode(Box<hkbNode<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbNodeInternalStateInfo(Box<hkbNodeInternalStateInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbParticleSystemEventPayload(Box<hkbParticleSystemEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbPoseMatchingGenerator(Box<hkbPoseMatchingGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbPoseMatchingGeneratorInternalState(
        Box<hkbPoseMatchingGeneratorInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbPoweredRagdollControlData(Box<hkbPoweredRagdollControlData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbPoweredRagdollControlsModifier(Box<hkbPoweredRagdollControlsModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbProjectData(Box<hkbProjectData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbProjectStringData(Box<hkbProjectStringData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbProxyModifier(Box<hkbProxyModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbProxyModifierProxyInfo(Box<hkbProxyModifierProxyInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRaiseEventCommand(Box<hkbRaiseEventCommand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRealEventPayload(Box<hkbRealEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRealVariableSequencedData(Box<hkbRealVariableSequencedData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRealVariableSequencedDataSample(Box<hkbRealVariableSequencedDataSample<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbReferencePoseGenerator(Box<hkbReferencePoseGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRegisteredGenerator(Box<hkbRegisteredGenerator<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRigidBodyRagdollControlData(Box<hkbRigidBodyRagdollControlData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRigidBodyRagdollControlsModifier(Box<hkbRigidBodyRagdollControlsModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRoleAttribute(Box<hkbRoleAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRotateCharacterModifier(Box<hkbRotateCharacterModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbRotateCharacterModifierInternalState(
        Box<hkbRotateCharacterModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSenseHandleModifier(Box<hkbSenseHandleModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSenseHandleModifierRange(Box<hkbSenseHandleModifierRange<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSequence(Box<hkbSequence<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSequencedData(Box<hkbSequencedData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSequenceInternalState(Box<hkbSequenceInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSequenceStringData(Box<hkbSequenceStringData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSetBehaviorCommand(Box<hkbSetBehaviorCommand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSetLocalTimeOfClipGeneratorCommand(
        Box<hkbSetLocalTimeOfClipGeneratorCommand<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSetNodePropertyCommand(Box<hkbSetNodePropertyCommand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSetWordVariableCommand(Box<hkbSetWordVariableCommand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSetWorldFromModelModifier(Box<hkbSetWorldFromModelModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSimulationControlCommand(Box<hkbSimulationControlCommand<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbSimulationStateInfo(Box<hkbSimulationStateInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateChooser(Box<hkbStateChooser<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateListener(Box<hkbStateListener<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachine(Box<hkbStateMachine<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineActiveTransitionInfo(Box<hkbStateMachineActiveTransitionInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineDelayedTransitionInfo(Box<hkbStateMachineDelayedTransitionInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineEventPropertyArray(Box<hkbStateMachineEventPropertyArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineInternalState(Box<hkbStateMachineInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineNestedStateMachineData(
        Box<hkbStateMachineNestedStateMachineData<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineProspectiveTransitionInfo(
        Box<hkbStateMachineProspectiveTransitionInfo<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineStateInfo(Box<hkbStateMachineStateInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineTimeInterval(Box<hkbStateMachineTimeInterval<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineTransitionInfo(Box<hkbStateMachineTransitionInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineTransitionInfoArray(Box<hkbStateMachineTransitionInfoArray<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStateMachineTransitionInfoReference(
        Box<hkbStateMachineTransitionInfoReference<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStringCondition(Box<hkbStringCondition<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbStringEventPayload(Box<hkbStringEventPayload<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTestStateChooser(Box<hkbTestStateChooser<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTimerModifier(Box<hkbTimerModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTimerModifierInternalState(Box<hkbTimerModifierInternalState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTransformVectorModifier(Box<hkbTransformVectorModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTransformVectorModifierInternalState(
        Box<hkbTransformVectorModifierInternalState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTransitionEffect(Box<hkbTransitionEffect<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbTwistModifier(Box<hkbTwistModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbVariableBindingSet(Box<hkbVariableBindingSet<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbVariableBindingSetBinding(Box<hkbVariableBindingSetBinding<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbVariableInfo(Box<hkbVariableInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbVariableValue(Box<hkbVariableValue<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbVariableValueSet(Box<hkbVariableValueSet<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbWorldEnums(Box<hkbWorldEnums<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkbWorldFromModelModeData(Box<hkbWorldFromModelModeData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkClass(Box<hkClass<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkClassEnum(Box<hkClassEnum<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkClassEnumItem(Box<hkClassEnumItem<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkClassMember(Box<hkClassMember<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkColor(Box<hkColor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkContactPoint(Box<hkContactPoint<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkContactPointMaterial(Box<hkContactPointMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkCustomAttributes(Box<hkCustomAttributes<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkCustomAttributesAttribute(Box<hkCustomAttributesAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkDataObjectTypeAttribute(Box<hkDataObjectTypeAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkDescriptionAttribute(Box<hkDescriptionAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkDocumentationAttribute(Box<hkDocumentationAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkGeometry(Box<hkGeometry<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkGeometryTriangle(Box<hkGeometryTriangle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkGizmoAttribute(Box<hkGizmoAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkHalf8(Box<hkHalf8<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkIndexedTransformSet(Box<hkIndexedTransformSet<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkLinkAttribute(Box<hkLinkAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkLocalFrame(Box<hkLocalFrame<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkLocalFrameGroup(Box<hkLocalFrameGroup<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryMeshBody(Box<hkMemoryMeshBody<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryMeshMaterial(Box<hkMemoryMeshMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryMeshShape(Box<hkMemoryMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryMeshTexture(Box<hkMemoryMeshTexture<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryMeshVertexBuffer(Box<hkMemoryMeshVertexBuffer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryResourceContainer(Box<hkMemoryResourceContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryResourceHandle(Box<hkMemoryResourceHandle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryResourceHandleExternalLink(Box<hkMemoryResourceHandleExternalLink<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMemoryTrackerAttribute(Box<hkMemoryTrackerAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshBody(Box<hkMeshBody<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshBoneIndexMapping(Box<hkMeshBoneIndexMapping<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshMaterial(Box<hkMeshMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshSection(Box<hkMeshSection<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshSectionCinfo(Box<hkMeshSectionCinfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshShape(Box<hkMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshTexture(Box<hkMeshTexture<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMeshVertexBuffer(Box<hkMeshVertexBuffer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkModelerNodeTypeAttribute(Box<hkModelerNodeTypeAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMonitorStreamColorTable(Box<hkMonitorStreamColorTable<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMonitorStreamColorTableColorPair(Box<hkMonitorStreamColorTableColorPair<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMonitorStreamFrameInfo(Box<hkMonitorStreamFrameInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMonitorStreamStringMap(Box<hkMonitorStreamStringMap<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMonitorStreamStringMapStringMap(Box<hkMonitorStreamStringMapStringMap<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMoppBvTreeShapeBase(Box<hkMoppBvTreeShapeBase<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMotionState(Box<hkMotionState<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMultipleVertexBuffer(Box<hkMultipleVertexBuffer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMultipleVertexBufferElementInfo(Box<hkMultipleVertexBufferElementInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMultipleVertexBufferLockedElement(Box<hkMultipleVertexBufferLockedElement<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMultipleVertexBufferVertexBufferInfo(
        Box<hkMultipleVertexBufferVertexBufferInfo<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkMultiThreadCheck(Box<hkMultiThreadCheck<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkp2dAngConstraintAtom(Box<hkp2dAngConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAabbPhantom(Box<hkpAabbPhantom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkPackedVector3(Box<hkPackedVector3<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkPackfileHeader(Box<hkPackfileHeader<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkPackfileSectionHeader(Box<hkPackfileSectionHeader<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAction(Box<hkpAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAgent1nSector(Box<hkpAgent1nSector<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAngConstraintAtom(Box<hkpAngConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAngFrictionConstraintAtom(Box<hkpAngFrictionConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAngLimitConstraintAtom(Box<hkpAngLimitConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAngMotorConstraintAtom(Box<hkpAngMotorConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpAngularDashpotAction(Box<hkpAngularDashpotAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpArrayAction(Box<hkpArrayAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBallAndSocketConstraintData(Box<hkpBallAndSocketConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBallAndSocketConstraintDataAtoms(Box<hkpBallAndSocketConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBallGun(Box<hkpBallGun<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBallSocketChainData(Box<hkpBallSocketChainData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBallSocketChainDataConstraintInfo(Box<hkpBallSocketChainDataConstraintInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBallSocketConstraintAtom(Box<hkpBallSocketConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBinaryAction(Box<hkpBinaryAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBoxMotion(Box<hkpBoxMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBoxShape(Box<hkpBoxShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBreakableBody(Box<hkpBreakableBody<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBreakableConstraintData(Box<hkpBreakableConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBridgeAtoms(Box<hkpBridgeAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBridgeConstraintAtom(Box<hkpBridgeConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBroadPhaseHandle(Box<hkpBroadPhaseHandle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBvShape(Box<hkpBvShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpBvTreeShape(Box<hkpBvTreeShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCachingShapePhantom(Box<hkpCachingShapePhantom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCallbackConstraintMotor(Box<hkpCallbackConstraintMotor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCapsuleShape(Box<hkpCapsuleShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCdBody(Box<hkpCdBody<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCenterOfMassChangerModifierConstraintAtom(
        Box<hkpCenterOfMassChangerModifierConstraintAtom<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCharacterControllerCinfo(Box<hkpCharacterControllerCinfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCharacterMotion(Box<hkpCharacterMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCharacterProxyCinfo(Box<hkpCharacterProxyCinfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCharacterRigidBodyCinfo(Box<hkpCharacterRigidBodyCinfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCogWheelConstraintAtom(Box<hkpCogWheelConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCogWheelConstraintData(Box<hkpCogWheelConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCogWheelConstraintDataAtoms(Box<hkpCogWheelConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCollidable(Box<hkpCollidable<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCollidableBoundingVolumeData(Box<hkpCollidableBoundingVolumeData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCollidableCollidableFilter(Box<hkpCollidableCollidableFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCollisionFilter(Box<hkpCollisionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCollisionFilterList(Box<hkpCollisionFilterList<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCompressedMeshShape(Box<hkpCompressedMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCompressedMeshShapeBigTriangle(Box<hkpCompressedMeshShapeBigTriangle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCompressedMeshShapeChunk(Box<hkpCompressedMeshShapeChunk<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCompressedMeshShapeConvexPiece(Box<hkpCompressedMeshShapeConvexPiece<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCompressedSampledHeightFieldShape(Box<hkpCompressedSampledHeightFieldShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConeLimitConstraintAtom(Box<hkpConeLimitConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstrainedSystemFilter(Box<hkpConstrainedSystemFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintAtom(Box<hkpConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintChainData(Box<hkpConstraintChainData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintChainInstance(Box<hkpConstraintChainInstance<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintChainInstanceAction(Box<hkpConstraintChainInstanceAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintCollisionFilter(Box<hkpConstraintCollisionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintData(Box<hkpConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintInstance(Box<hkpConstraintInstance<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintInstanceSmallArraySerializeOverrideType(
        Box<hkpConstraintInstanceSmallArraySerializeOverrideType<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConstraintMotor(Box<hkpConstraintMotor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexListFilter(Box<hkpConvexListFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexListShape(Box<hkpConvexListShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexPieceMeshShape(Box<hkpConvexPieceMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexPieceStreamData(Box<hkpConvexPieceStreamData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexShape(Box<hkpConvexShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexTransformShape(Box<hkpConvexTransformShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexTransformShapeBase(Box<hkpConvexTransformShapeBase<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexTranslateShape(Box<hkpConvexTranslateShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexVerticesConnectivity(Box<hkpConvexVerticesConnectivity<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexVerticesShape(Box<hkpConvexVerticesShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpConvexVerticesShapeFourVectors(Box<hkpConvexVerticesShapeFourVectors<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpCylinderShape(Box<hkpCylinderShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDashpotAction(Box<hkpDashpotAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDefaultConvexListFilter(Box<hkpDefaultConvexListFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDefaultWorldMemoryWatchDog(Box<hkpDefaultWorldMemoryWatchDog<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDisableEntityCollisionFilter(Box<hkpDisableEntityCollisionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDisplayBindingData(Box<hkpDisplayBindingData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDisplayBindingDataPhysicsSystem(Box<hkpDisplayBindingDataPhysicsSystem<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpDisplayBindingDataRigidBody(Box<hkpDisplayBindingDataRigidBody<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpEntity(Box<hkpEntity<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpEntityExtendedListeners(Box<hkpEntityExtendedListeners<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpEntitySmallArraySerializeOverrideType(
        Box<hkpEntitySmallArraySerializeOverrideType<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpEntitySpuCollisionCallback(Box<hkpEntitySpuCollisionCallback<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpExtendedMeshShape(Box<hkpExtendedMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpExtendedMeshShapeShapesSubpart(Box<hkpExtendedMeshShapeShapesSubpart<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpExtendedMeshShapeSubpart(Box<hkpExtendedMeshShapeSubpart<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpExtendedMeshShapeTrianglesSubpart(Box<hkpExtendedMeshShapeTrianglesSubpart<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpFastMeshShape(Box<hkpFastMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpFirstPersonGun(Box<hkpFirstPersonGun<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpFixedRigidMotion(Box<hkpFixedRigidMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpGenericConstraintData(Box<hkpGenericConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpGenericConstraintDataScheme(Box<hkpGenericConstraintDataScheme<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpGenericConstraintDataSchemeConstraintInfo(
        Box<hkpGenericConstraintDataSchemeConstraintInfo<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpGravityGun(Box<hkpGravityGun<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpGroupCollisionFilter(Box<hkpGroupCollisionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpGroupFilter(Box<hkpGroupFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpHeightFieldShape(Box<hkpHeightFieldShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpHingeConstraintData(Box<hkpHingeConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpHingeConstraintDataAtoms(Box<hkpHingeConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpHingeLimitsData(Box<hkpHingeLimitsData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpHingeLimitsDataAtoms(Box<hkpHingeLimitsDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpIgnoreModifierConstraintAtom(Box<hkpIgnoreModifierConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpKeyframedRigidMotion(Box<hkpKeyframedRigidMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLimitedForceConstraintMotor(Box<hkpLimitedForceConstraintMotor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLimitedHingeConstraintData(Box<hkpLimitedHingeConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLimitedHingeConstraintDataAtoms(Box<hkpLimitedHingeConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinConstraintAtom(Box<hkpLinConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinearParametricCurve(Box<hkpLinearParametricCurve<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinFrictionConstraintAtom(Box<hkpLinFrictionConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinkedCollidable(Box<hkpLinkedCollidable<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinLimitConstraintAtom(Box<hkpLinLimitConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinMotorConstraintAtom(Box<hkpLinMotorConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpLinSoftConstraintAtom(Box<hkpLinSoftConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpListShape(Box<hkpListShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpListShapeChildInfo(Box<hkpListShapeChildInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMalleableConstraintData(Box<hkpMalleableConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMassChangerModifierConstraintAtom(Box<hkpMassChangerModifierConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMassProperties(Box<hkpMassProperties<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMaterial(Box<hkpMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMaxSizeMotion(Box<hkpMaxSizeMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMeshMaterial(Box<hkpMeshMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMeshShape(Box<hkpMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMeshShapeSubpart(Box<hkpMeshShapeSubpart<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpModifierConstraintAtom(Box<hkpModifierConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMoppBvTreeShape(Box<hkpMoppBvTreeShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMoppCode(Box<hkpMoppCode<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMoppCodeCodeInfo(Box<hkpMoppCodeCodeInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMoppCodeReindexedTerminal(Box<hkpMoppCodeReindexedTerminal<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMotion(Box<hkpMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMotorAction(Box<hkpMotorAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMountedBallGun(Box<hkpMountedBallGun<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMouseSpringAction(Box<hkpMouseSpringAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMovingSurfaceModifierConstraintAtom(
        Box<hkpMovingSurfaceModifierConstraintAtom<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMultiRayShape(Box<hkpMultiRayShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMultiRayShapeRay(Box<hkpMultiRayShapeRay<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMultiSphereShape(Box<hkpMultiSphereShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpMultithreadedVehicleManager(Box<hkpMultithreadedVehicleManager<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpNamedMeshMaterial(Box<hkpNamedMeshMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpNullCollisionFilter(Box<hkpNullCollisionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkPostFinishAttribute(Box<hkPostFinishAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpOverwritePivotConstraintAtom(Box<hkpOverwritePivotConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPairCollisionFilter(Box<hkpPairCollisionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPairCollisionFilterMapPairFilterKeyOverrideType(
        Box<hkpPairCollisionFilterMapPairFilterKeyOverrideType<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpParametricCurve(Box<hkpParametricCurve<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPhantom(Box<hkpPhantom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPhantomCallbackShape(Box<hkpPhantomCallbackShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPhysicsData(Box<hkpPhysicsData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPhysicsSystem(Box<hkpPhysicsSystem<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPhysicsSystemWithContacts(Box<hkpPhysicsSystemWithContacts<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPlaneShape(Box<hkpPlaneShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPointToPathConstraintData(Box<hkpPointToPathConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPointToPlaneConstraintData(Box<hkpPointToPlaneConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPointToPlaneConstraintDataAtoms(Box<hkpPointToPlaneConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPositionConstraintMotor(Box<hkpPositionConstraintMotor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPoweredChainData(Box<hkpPoweredChainData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPoweredChainDataConstraintInfo(Box<hkpPoweredChainDataConstraintInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPoweredChainMapper(Box<hkpPoweredChainMapper<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPoweredChainMapperLinkInfo(Box<hkpPoweredChainMapperLinkInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPoweredChainMapperTarget(Box<hkpPoweredChainMapperTarget<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPrismaticConstraintData(Box<hkpPrismaticConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPrismaticConstraintDataAtoms(Box<hkpPrismaticConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpProjectileGun(Box<hkpProjectileGun<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpProperty(Box<hkpProperty<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPropertyValue(Box<hkpPropertyValue<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPulleyConstraintAtom(Box<hkpPulleyConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPulleyConstraintData(Box<hkpPulleyConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpPulleyConstraintDataAtoms(Box<hkpPulleyConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRackAndPinionConstraintAtom(Box<hkpRackAndPinionConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRackAndPinionConstraintData(Box<hkpRackAndPinionConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRackAndPinionConstraintDataAtoms(Box<hkpRackAndPinionConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRagdollConstraintData(Box<hkpRagdollConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRagdollConstraintDataAtoms(Box<hkpRagdollConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRagdollLimitsData(Box<hkpRagdollLimitsData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRagdollLimitsDataAtoms(Box<hkpRagdollLimitsDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRagdollMotorConstraintAtom(Box<hkpRagdollMotorConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRayCollidableFilter(Box<hkpRayCollidableFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRayShapeCollectionFilter(Box<hkpRayShapeCollectionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRejectChassisListener(Box<hkpRejectChassisListener<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRemoveTerminalsMoppModifier(Box<hkpRemoveTerminalsMoppModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpReorientAction(Box<hkpReorientAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRigidBody(Box<hkpRigidBody<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRotationalConstraintData(Box<hkpRotationalConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpRotationalConstraintDataAtoms(Box<hkpRotationalConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSampledHeightFieldShape(Box<hkpSampledHeightFieldShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedAgentNnEntry(Box<hkpSerializedAgentNnEntry<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedDisplayMarker(Box<hkpSerializedDisplayMarker<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedDisplayMarkerList(Box<hkpSerializedDisplayMarkerList<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedDisplayRbTransforms(Box<hkpSerializedDisplayRbTransforms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedDisplayRbTransformsDisplayTransformPair(
        Box<hkpSerializedDisplayRbTransformsDisplayTransformPair<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedSubTrack1nInfo(Box<hkpSerializedSubTrack1nInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSerializedTrack1nInfo(Box<hkpSerializedTrack1nInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSetLocalRotationsConstraintAtom(Box<hkpSetLocalRotationsConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSetLocalTransformsConstraintAtom(Box<hkpSetLocalTransformsConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSetLocalTranslationsConstraintAtom(
        Box<hkpSetLocalTranslationsConstraintAtom<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSetupStabilizationAtom(Box<hkpSetupStabilizationAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShape(Box<hkpShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShapeCollection(Box<hkpShapeCollection<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShapeCollectionFilter(Box<hkpShapeCollectionFilter<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShapeContainer(Box<hkpShapeContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShapeInfo(Box<hkpShapeInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShapeModifier(Box<hkpShapeModifier<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpShapePhantom(Box<hkpShapePhantom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimpleContactConstraintAtom(Box<hkpSimpleContactConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimpleContactConstraintDataInfo(Box<hkpSimpleContactConstraintDataInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimpleMeshShape(Box<hkpSimpleMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimpleMeshShapeTriangle(Box<hkpSimpleMeshShapeTriangle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimpleShapePhantom(Box<hkpSimpleShapePhantom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimpleShapePhantomCollisionDetail(Box<hkpSimpleShapePhantomCollisionDetail<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSimulation(Box<hkpSimulation<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSingleShapeContainer(Box<hkpSingleShapeContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSoftContactModifierConstraintAtom(Box<hkpSoftContactModifierConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSphereMotion(Box<hkpSphereMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSphereRepShape(Box<hkpSphereRepShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSphereShape(Box<hkpSphereShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSpringAction(Box<hkpSpringAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpSpringDamperConstraintMotor(Box<hkpSpringDamperConstraintMotor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStiffSpringChainData(Box<hkpStiffSpringChainData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStiffSpringChainDataConstraintInfo(
        Box<hkpStiffSpringChainDataConstraintInfo<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStiffSpringConstraintAtom(Box<hkpStiffSpringConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStiffSpringConstraintData(Box<hkpStiffSpringConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStiffSpringConstraintDataAtoms(Box<hkpStiffSpringConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageExtendedMeshShape(Box<hkpStorageExtendedMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageExtendedMeshShapeMaterial(Box<hkpStorageExtendedMeshShapeMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageExtendedMeshShapeMeshSubpartStorage(
        Box<hkpStorageExtendedMeshShapeMeshSubpartStorage<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageExtendedMeshShapeShapeSubpartStorage(
        Box<hkpStorageExtendedMeshShapeShapeSubpartStorage<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageMeshShape(Box<hkpStorageMeshShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageMeshShapeSubpartStorage(Box<hkpStorageMeshShapeSubpartStorage<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpStorageSampledHeightFieldShape(Box<hkpStorageSampledHeightFieldShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpThinBoxMotion(Box<hkpThinBoxMotion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTransformShape(Box<hkpTransformShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTriangleShape(Box<hkpTriangleShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTriggerVolume(Box<hkpTriggerVolume<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTriggerVolumeEventInfo(Box<hkpTriggerVolumeEventInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTriSampledHeightFieldBvTreeShape(Box<hkpTriSampledHeightFieldBvTreeShape<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTriSampledHeightFieldCollection(Box<hkpTriSampledHeightFieldCollection<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTwistLimitConstraintAtom(Box<hkpTwistLimitConstraintAtom<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTypedBroadPhaseHandle(Box<hkpTypedBroadPhaseHandle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTyremarkPoint(Box<hkpTyremarkPoint<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTyremarksInfo(Box<hkpTyremarksInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpTyremarksWheel(Box<hkpTyremarksWheel<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpUnaryAction(Box<hkpUnaryAction<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleAerodynamics(Box<hkpVehicleAerodynamics<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleBrake(Box<hkpVehicleBrake<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleCastBatchingManager(Box<hkpVehicleCastBatchingManager<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleData(Box<hkpVehicleData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDataWheelComponentParams(Box<hkpVehicleDataWheelComponentParams<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultAerodynamics(Box<hkpVehicleDefaultAerodynamics<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultAnalogDriverInput(Box<hkpVehicleDefaultAnalogDriverInput<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultBrake(Box<hkpVehicleDefaultBrake<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultBrakeWheelBrakingProperties(
        Box<hkpVehicleDefaultBrakeWheelBrakingProperties<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultEngine(Box<hkpVehicleDefaultEngine<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultSteering(Box<hkpVehicleDefaultSteering<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultSuspension(Box<hkpVehicleDefaultSuspension<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters(
        Box<hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultTransmission(Box<hkpVehicleDefaultTransmission<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDefaultVelocityDamper(Box<hkpVehicleDefaultVelocityDamper<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDriverInput(Box<hkpVehicleDriverInput<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDriverInputAnalogStatus(Box<hkpVehicleDriverInputAnalogStatus<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleDriverInputStatus(Box<hkpVehicleDriverInputStatus<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleEngine(Box<hkpVehicleEngine<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleFrictionDescription(Box<hkpVehicleFrictionDescription<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleFrictionDescriptionAxisDescription(
        Box<hkpVehicleFrictionDescriptionAxisDescription<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleFrictionStatus(Box<hkpVehicleFrictionStatus<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleFrictionStatusAxisStatus(Box<hkpVehicleFrictionStatusAxisStatus<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleInstance(Box<hkpVehicleInstance<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleInstanceWheelInfo(Box<hkpVehicleInstanceWheelInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleLinearCastBatchingManager(Box<hkpVehicleLinearCastBatchingManager<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleLinearCastWheelCollide(Box<hkpVehicleLinearCastWheelCollide<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleLinearCastWheelCollideWheelState(
        Box<hkpVehicleLinearCastWheelCollideWheelState<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleManager(Box<hkpVehicleManager<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleRayCastBatchingManager(Box<hkpVehicleRayCastBatchingManager<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleRayCastWheelCollide(Box<hkpVehicleRayCastWheelCollide<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleSteering(Box<hkpVehicleSteering<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleSuspension(Box<hkpVehicleSuspension<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleSuspensionSuspensionWheelParameters(
        Box<hkpVehicleSuspensionSuspensionWheelParameters<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleTransmission(Box<hkpVehicleTransmission<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleVelocityDamper(Box<hkpVehicleVelocityDamper<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVehicleWheelCollide(Box<hkpVehicleWheelCollide<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpVelocityConstraintMotor(Box<hkpVelocityConstraintMotor<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpViscousSurfaceModifierConstraintAtom(
        Box<hkpViscousSurfaceModifierConstraintAtom<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpWeldingUtility(Box<hkpWeldingUtility<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpWheelConstraintData(Box<hkpWheelConstraintData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpWheelConstraintDataAtoms(Box<hkpWheelConstraintDataAtoms<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpWorld(Box<hkpWorld<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpWorldCinfo(Box<hkpWorldCinfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkpWorldObject(Box<hkpWorldObject<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkQTransform(Box<hkQTransform<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkRangeInt32Attribute(Box<hkRangeInt32Attribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkRangeRealAttribute(Box<hkRangeRealAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkReferencedObject(Box<hkReferencedObject<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkReflectedFileAttribute(Box<hkReflectedFileAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkResourceBase(Box<hkResourceBase<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkResourceContainer(Box<hkResourceContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkResourceHandle(Box<hkResourceHandle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkRootLevelContainer(Box<hkRootLevelContainer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkRootLevelContainerNamedVariant(Box<hkRootLevelContainerNamedVariant<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkSemanticsAttribute(Box<hkSemanticsAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkSimpleLocalFrame(Box<hkSimpleLocalFrame<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkSphere(Box<hkSphere<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkSweptTransform(Box<hkSweptTransform<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkTraceStreamTitle(Box<hkTraceStreamTitle<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkTrackerSerializableScanSnapshot(Box<hkTrackerSerializableScanSnapshot<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkTrackerSerializableScanSnapshotAllocation(
        Box<hkTrackerSerializableScanSnapshotAllocation<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkTrackerSerializableScanSnapshotBlock(
        Box<hkTrackerSerializableScanSnapshotBlock<'a>>,
    ),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkUiAttribute(Box<hkUiAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkVertexFormat(Box<hkVertexFormat<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkVertexFormatElement(Box<hkVertexFormatElement<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkWorldMemoryAvailableWatchDog(Box<hkWorldMemoryAvailableWatchDog<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAnimatedFloat(Box<hkxAnimatedFloat<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAnimatedMatrix(Box<hkxAnimatedMatrix<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAnimatedQuaternion(Box<hkxAnimatedQuaternion<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAnimatedVector(Box<hkxAnimatedVector<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAttribute(Box<hkxAttribute<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAttributeGroup(Box<hkxAttributeGroup<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxAttributeHolder(Box<hkxAttributeHolder<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxCamera(Box<hkxCamera<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxEdgeSelectionChannel(Box<hkxEdgeSelectionChannel<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxEnum(Box<hkxEnum<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxEnumItem(Box<hkxEnumItem<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxEnvironment(Box<hkxEnvironment<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxEnvironmentVariable(Box<hkxEnvironmentVariable<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxIndexBuffer(Box<hkxIndexBuffer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxLight(Box<hkxLight<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMaterial(Box<hkxMaterial<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMaterialEffect(Box<hkxMaterialEffect<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMaterialProperty(Box<hkxMaterialProperty<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMaterialShader(Box<hkxMaterialShader<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMaterialShaderSet(Box<hkxMaterialShaderSet<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMaterialTextureStage(Box<hkxMaterialTextureStage<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMesh(Box<hkxMesh<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMeshSection(Box<hkxMeshSection<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxMeshUserChannelInfo(Box<hkxMeshUserChannelInfo<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxNode(Box<hkxNode<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxNodeAnnotationData(Box<hkxNodeAnnotationData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxNodeSelectionSet(Box<hkxNodeSelectionSet<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxScene(Box<hkxScene<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxSkinBinding(Box<hkxSkinBinding<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxSparselyAnimatedBool(Box<hkxSparselyAnimatedBool<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxSparselyAnimatedEnum(Box<hkxSparselyAnimatedEnum<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxSparselyAnimatedInt(Box<hkxSparselyAnimatedInt<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxSparselyAnimatedString(Box<hkxSparselyAnimatedString<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxTextureFile(Box<hkxTextureFile<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxTextureInplace(Box<hkxTextureInplace<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxTriangleSelectionChannel(Box<hkxTriangleSelectionChannel<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexBuffer(Box<hkxVertexBuffer<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexBufferVertexData(Box<hkxVertexBufferVertexData<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexDescription(Box<hkxVertexDescription<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexDescriptionElementDecl(Box<hkxVertexDescriptionElementDecl<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexFloatDataChannel(Box<hkxVertexFloatDataChannel<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexIntDataChannel(Box<hkxVertexIntDataChannel<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexSelectionChannel(Box<hkxVertexSelectionChannel<'a>>),
    #[cfg_attr(feature = "serde", serde(borrow))]
    hkxVertexVectorDataChannel(Box<hkxVertexVectorDataChannel<'a>>),
}
impl Default for Classes<'_> {
    fn default() -> Self {
        Self::BGSGamebryoSequenceGenerator(
            Box::new(BGSGamebryoSequenceGenerator::default()),
        )
    }
}
impl _serde::HavokClass for Classes<'_> {
    fn name(&self) -> &'static str {
        match &self {
            Classes::BGSGamebryoSequenceGenerator(class) => class.name(),
            Classes::BSBoneSwitchGenerator(class) => class.name(),
            Classes::BSBoneSwitchGeneratorBoneData(class) => class.name(),
            Classes::BSComputeAddBoneAnimModifier(class) => class.name(),
            Classes::BSCyclicBlendTransitionGenerator(class) => class.name(),
            Classes::BSDecomposeVectorModifier(class) => class.name(),
            Classes::BSDirectAtModifier(class) => class.name(),
            Classes::BSDistTriggerModifier(class) => class.name(),
            Classes::BSEventEveryNEventsModifier(class) => class.name(),
            Classes::BSEventOnDeactivateModifier(class) => class.name(),
            Classes::BSEventOnFalseToTrueModifier(class) => class.name(),
            Classes::BSGetTimeStepModifier(class) => class.name(),
            Classes::BSInterpValueModifier(class) => class.name(),
            Classes::BSIsActiveModifier(class) => class.name(),
            Classes::BSIStateManagerModifier(class) => class.name(),
            Classes::BSIStateManagerModifierBSiStateData(class) => class.name(),
            Classes::BSIStateManagerModifierBSIStateManagerStateListener(class) => {
                class.name()
            }
            Classes::BSiStateTaggingGenerator(class) => class.name(),
            Classes::BSLimbIKModifier(class) => class.name(),
            Classes::BSLookAtModifier(class) => class.name(),
            Classes::BSLookAtModifierBoneData(class) => class.name(),
            Classes::BSModifyOnceModifier(class) => class.name(),
            Classes::BSOffsetAnimationGenerator(class) => class.name(),
            Classes::BSPassByTargetTriggerModifier(class) => class.name(),
            Classes::BSRagdollContactListenerModifier(class) => class.name(),
            Classes::BSSpeedSamplerModifier(class) => class.name(),
            Classes::BSSynchronizedClipGenerator(class) => class.name(),
            Classes::BSTimerModifier(class) => class.name(),
            Classes::BSTweenerModifier(class) => class.name(),
            Classes::hkAabb(class) => class.name(),
            Classes::hkAabbHalf(class) => class.name(),
            Classes::hkAabbUint32(class) => class.name(),
            Classes::hkaAnimatedReferenceFrame(class) => class.name(),
            Classes::hkaAnimation(class) => class.name(),
            Classes::hkaAnimationBinding(class) => class.name(),
            Classes::hkaAnimationContainer(class) => class.name(),
            Classes::hkaAnimationPreviewColorContainer(class) => class.name(),
            Classes::hkaAnnotationTrack(class) => class.name(),
            Classes::hkaAnnotationTrackAnnotation(class) => class.name(),
            Classes::hkaBone(class) => class.name(),
            Classes::hkaBoneAttachment(class) => class.name(),
            Classes::hkaDefaultAnimatedReferenceFrame(class) => class.name(),
            Classes::hkaDeltaCompressedAnimation(class) => class.name(),
            Classes::hkaDeltaCompressedAnimationQuantizationFormat(class) => class.name(),
            Classes::hkaFootstepAnalysisInfo(class) => class.name(),
            Classes::hkaFootstepAnalysisInfoContainer(class) => class.name(),
            Classes::hkaInterleavedUncompressedAnimation(class) => class.name(),
            Classes::hkaKeyFrameHierarchyUtility(class) => class.name(),
            Classes::hkaKeyFrameHierarchyUtilityControlData(class) => class.name(),
            Classes::hkAlignSceneToNodeOptions(class) => class.name(),
            Classes::hkaMeshBinding(class) => class.name(),
            Classes::hkaMeshBindingMapping(class) => class.name(),
            Classes::hkaQuantizedAnimation(class) => class.name(),
            Classes::hkaQuantizedAnimationTrackCompressionParams(class) => class.name(),
            Classes::hkaRagdollInstance(class) => class.name(),
            Classes::hkArrayTypeAttribute(class) => class.name(),
            Classes::hkaSkeleton(class) => class.name(),
            Classes::hkaSkeletonLocalFrameOnBone(class) => class.name(),
            Classes::hkaSkeletonMapper(class) => class.name(),
            Classes::hkaSkeletonMapperData(class) => class.name(),
            Classes::hkaSkeletonMapperDataChainMapping(class) => class.name(),
            Classes::hkaSkeletonMapperDataSimpleMapping(class) => class.name(),
            Classes::hkaSplineCompressedAnimation(class) => class.name(),
            Classes::hkaSplineCompressedAnimationAnimationCompressionParams(class) => {
                class.name()
            }
            Classes::hkaSplineCompressedAnimationTrackCompressionParams(class) => {
                class.name()
            }
            Classes::hkaWaveletCompressedAnimation(class) => class.name(),
            Classes::hkaWaveletCompressedAnimationCompressionParams(class) => {
                class.name()
            }
            Classes::hkaWaveletCompressedAnimationQuantizationFormat(class) => {
                class.name()
            }
            Classes::hkBaseObject(class) => class.name(),
            Classes::hkbAttachmentModifier(class) => class.name(),
            Classes::hkbAttachmentSetup(class) => class.name(),
            Classes::hkbAttributeModifier(class) => class.name(),
            Classes::hkbAttributeModifierAssignment(class) => class.name(),
            Classes::hkbAuxiliaryNodeInfo(class) => class.name(),
            Classes::hkbBehaviorEventsInfo(class) => class.name(),
            Classes::hkbBehaviorGraph(class) => class.name(),
            Classes::hkbBehaviorGraphData(class) => class.name(),
            Classes::hkbBehaviorGraphInternalState(class) => class.name(),
            Classes::hkbBehaviorGraphInternalStateInfo(class) => class.name(),
            Classes::hkbBehaviorGraphStringData(class) => class.name(),
            Classes::hkbBehaviorInfo(class) => class.name(),
            Classes::hkbBehaviorInfoIdToNamePair(class) => class.name(),
            Classes::hkbBehaviorReferenceGenerator(class) => class.name(),
            Classes::hkbBindable(class) => class.name(),
            Classes::hkbBlendCurveUtils(class) => class.name(),
            Classes::hkbBlenderGenerator(class) => class.name(),
            Classes::hkbBlenderGeneratorChild(class) => class.name(),
            Classes::hkbBlenderGeneratorChildInternalState(class) => class.name(),
            Classes::hkbBlenderGeneratorInternalState(class) => class.name(),
            Classes::hkbBlendingTransitionEffect(class) => class.name(),
            Classes::hkbBlendingTransitionEffectInternalState(class) => class.name(),
            Classes::hkbBoneIndexArray(class) => class.name(),
            Classes::hkbBoneWeightArray(class) => class.name(),
            Classes::hkbBoolVariableSequencedData(class) => class.name(),
            Classes::hkbBoolVariableSequencedDataSample(class) => class.name(),
            Classes::hkbCameraShakeEventPayload(class) => class.name(),
            Classes::hkbCharacter(class) => class.name(),
            Classes::hkbCharacterAddedInfo(class) => class.name(),
            Classes::hkbCharacterControlCommand(class) => class.name(),
            Classes::hkbCharacterControllerControlData(class) => class.name(),
            Classes::hkbCharacterControllerModifier(class) => class.name(),
            Classes::hkbCharacterControllerModifierInternalState(class) => class.name(),
            Classes::hkbCharacterData(class) => class.name(),
            Classes::hkbCharacterDataCharacterControllerInfo(class) => class.name(),
            Classes::hkbCharacterInfo(class) => class.name(),
            Classes::hkbCharacterSetup(class) => class.name(),
            Classes::hkbCharacterSkinInfo(class) => class.name(),
            Classes::hkbCharacterSteppedInfo(class) => class.name(),
            Classes::hkbCharacterStringData(class) => class.name(),
            Classes::hkbClientCharacterState(class) => class.name(),
            Classes::hkbClipGenerator(class) => class.name(),
            Classes::hkbClipGeneratorEcho(class) => class.name(),
            Classes::hkbClipGeneratorInternalState(class) => class.name(),
            Classes::hkbClipTrigger(class) => class.name(),
            Classes::hkbClipTriggerArray(class) => class.name(),
            Classes::hkbCombineTransformsModifier(class) => class.name(),
            Classes::hkbCombineTransformsModifierInternalState(class) => class.name(),
            Classes::hkbCompiledExpressionSet(class) => class.name(),
            Classes::hkbCompiledExpressionSetToken(class) => class.name(),
            Classes::hkbComputeDirectionModifier(class) => class.name(),
            Classes::hkbComputeDirectionModifierInternalState(class) => class.name(),
            Classes::hkbComputeRotationFromAxisAngleModifier(class) => class.name(),
            Classes::hkbComputeRotationFromAxisAngleModifierInternalState(class) => {
                class.name()
            }
            Classes::hkbComputeRotationToTargetModifier(class) => class.name(),
            Classes::hkbComputeRotationToTargetModifierInternalState(class) => {
                class.name()
            }
            Classes::hkbCondition(class) => class.name(),
            Classes::hkbContext(class) => class.name(),
            Classes::hkbDampingModifier(class) => class.name(),
            Classes::hkbDampingModifierInternalState(class) => class.name(),
            Classes::hkbDefaultMessageLog(class) => class.name(),
            Classes::hkbDelayedModifier(class) => class.name(),
            Classes::hkbDelayedModifierInternalState(class) => class.name(),
            Classes::hkbDetectCloseToGroundModifier(class) => class.name(),
            Classes::hkbDetectCloseToGroundModifierInternalState(class) => class.name(),
            Classes::hkbEvaluateExpressionModifier(class) => class.name(),
            Classes::hkbEvaluateExpressionModifierInternalExpressionData(class) => {
                class.name()
            }
            Classes::hkbEvaluateExpressionModifierInternalState(class) => class.name(),
            Classes::hkbEvaluateHandleModifier(class) => class.name(),
            Classes::hkbEvent(class) => class.name(),
            Classes::hkbEventBase(class) => class.name(),
            Classes::hkbEventDrivenModifier(class) => class.name(),
            Classes::hkbEventDrivenModifierInternalState(class) => class.name(),
            Classes::hkbEventInfo(class) => class.name(),
            Classes::hkbEventPayload(class) => class.name(),
            Classes::hkbEventPayloadList(class) => class.name(),
            Classes::hkbEventProperty(class) => class.name(),
            Classes::hkbEventRaisedInfo(class) => class.name(),
            Classes::hkbEventRangeData(class) => class.name(),
            Classes::hkbEventRangeDataArray(class) => class.name(),
            Classes::hkbEventSequencedData(class) => class.name(),
            Classes::hkbEventSequencedDataSequencedEvent(class) => class.name(),
            Classes::hkbEventsFromRangeModifier(class) => class.name(),
            Classes::hkbEventsFromRangeModifierInternalState(class) => class.name(),
            Classes::hkbExpressionCondition(class) => class.name(),
            Classes::hkbExpressionData(class) => class.name(),
            Classes::hkbExpressionDataArray(class) => class.name(),
            Classes::hkbExtractRagdollPoseModifier(class) => class.name(),
            Classes::hkbFootIkControlData(class) => class.name(),
            Classes::hkbFootIkControlsModifier(class) => class.name(),
            Classes::hkbFootIkControlsModifierLeg(class) => class.name(),
            Classes::hkbFootIkDriverInfo(class) => class.name(),
            Classes::hkbFootIkDriverInfoLeg(class) => class.name(),
            Classes::hkbFootIkGains(class) => class.name(),
            Classes::hkbFootIkModifier(class) => class.name(),
            Classes::hkbFootIkModifierInternalLegData(class) => class.name(),
            Classes::hkbFootIkModifierLeg(class) => class.name(),
            Classes::hkbGenerator(class) => class.name(),
            Classes::hkbGeneratorOutputListener(class) => class.name(),
            Classes::hkbGeneratorSyncInfo(class) => class.name(),
            Classes::hkbGeneratorSyncInfoSyncPoint(class) => class.name(),
            Classes::hkbGeneratorTransitionEffect(class) => class.name(),
            Classes::hkbGeneratorTransitionEffectInternalState(class) => class.name(),
            Classes::hkbGetHandleOnBoneModifier(class) => class.name(),
            Classes::hkbGetUpModifier(class) => class.name(),
            Classes::hkbGetUpModifierInternalState(class) => class.name(),
            Classes::hkbGetWorldFromModelModifier(class) => class.name(),
            Classes::hkbGetWorldFromModelModifierInternalState(class) => class.name(),
            Classes::hkbHandIkControlData(class) => class.name(),
            Classes::hkbHandIkControlsModifier(class) => class.name(),
            Classes::hkbHandIkControlsModifierHand(class) => class.name(),
            Classes::hkbHandIkDriverInfo(class) => class.name(),
            Classes::hkbHandIkDriverInfoHand(class) => class.name(),
            Classes::hkbHandIkModifier(class) => class.name(),
            Classes::hkbHandIkModifierHand(class) => class.name(),
            Classes::hkbHandle(class) => class.name(),
            Classes::hkbIntEventPayload(class) => class.name(),
            Classes::hkbIntVariableSequencedData(class) => class.name(),
            Classes::hkbIntVariableSequencedDataSample(class) => class.name(),
            Classes::hkBitField(class) => class.name(),
            Classes::hkbKeyframeBonesModifier(class) => class.name(),
            Classes::hkbKeyframeBonesModifierKeyframeInfo(class) => class.name(),
            Classes::hkbLinkedSymbolInfo(class) => class.name(),
            Classes::hkbLookAtModifier(class) => class.name(),
            Classes::hkbLookAtModifierInternalState(class) => class.name(),
            Classes::hkbManualSelectorGenerator(class) => class.name(),
            Classes::hkbManualSelectorGeneratorInternalState(class) => class.name(),
            Classes::hkbMessageLog(class) => class.name(),
            Classes::hkbMirroredSkeletonInfo(class) => class.name(),
            Classes::hkbMirrorModifier(class) => class.name(),
            Classes::hkbModifier(class) => class.name(),
            Classes::hkbModifierGenerator(class) => class.name(),
            Classes::hkbModifierList(class) => class.name(),
            Classes::hkbModifierWrapper(class) => class.name(),
            Classes::hkbMoveCharacterModifier(class) => class.name(),
            Classes::hkbMoveCharacterModifierInternalState(class) => class.name(),
            Classes::hkbNamedEventPayload(class) => class.name(),
            Classes::hkbNamedIntEventPayload(class) => class.name(),
            Classes::hkbNamedRealEventPayload(class) => class.name(),
            Classes::hkbNamedStringEventPayload(class) => class.name(),
            Classes::hkbNode(class) => class.name(),
            Classes::hkbNodeInternalStateInfo(class) => class.name(),
            Classes::hkbParticleSystemEventPayload(class) => class.name(),
            Classes::hkbPoseMatchingGenerator(class) => class.name(),
            Classes::hkbPoseMatchingGeneratorInternalState(class) => class.name(),
            Classes::hkbPoweredRagdollControlData(class) => class.name(),
            Classes::hkbPoweredRagdollControlsModifier(class) => class.name(),
            Classes::hkbProjectData(class) => class.name(),
            Classes::hkbProjectStringData(class) => class.name(),
            Classes::hkbProxyModifier(class) => class.name(),
            Classes::hkbProxyModifierProxyInfo(class) => class.name(),
            Classes::hkbRaiseEventCommand(class) => class.name(),
            Classes::hkbRealEventPayload(class) => class.name(),
            Classes::hkbRealVariableSequencedData(class) => class.name(),
            Classes::hkbRealVariableSequencedDataSample(class) => class.name(),
            Classes::hkbReferencePoseGenerator(class) => class.name(),
            Classes::hkbRegisteredGenerator(class) => class.name(),
            Classes::hkbRigidBodyRagdollControlData(class) => class.name(),
            Classes::hkbRigidBodyRagdollControlsModifier(class) => class.name(),
            Classes::hkbRoleAttribute(class) => class.name(),
            Classes::hkbRotateCharacterModifier(class) => class.name(),
            Classes::hkbRotateCharacterModifierInternalState(class) => class.name(),
            Classes::hkbSenseHandleModifier(class) => class.name(),
            Classes::hkbSenseHandleModifierRange(class) => class.name(),
            Classes::hkbSequence(class) => class.name(),
            Classes::hkbSequencedData(class) => class.name(),
            Classes::hkbSequenceInternalState(class) => class.name(),
            Classes::hkbSequenceStringData(class) => class.name(),
            Classes::hkbSetBehaviorCommand(class) => class.name(),
            Classes::hkbSetLocalTimeOfClipGeneratorCommand(class) => class.name(),
            Classes::hkbSetNodePropertyCommand(class) => class.name(),
            Classes::hkbSetWordVariableCommand(class) => class.name(),
            Classes::hkbSetWorldFromModelModifier(class) => class.name(),
            Classes::hkbSimulationControlCommand(class) => class.name(),
            Classes::hkbSimulationStateInfo(class) => class.name(),
            Classes::hkbStateChooser(class) => class.name(),
            Classes::hkbStateListener(class) => class.name(),
            Classes::hkbStateMachine(class) => class.name(),
            Classes::hkbStateMachineActiveTransitionInfo(class) => class.name(),
            Classes::hkbStateMachineDelayedTransitionInfo(class) => class.name(),
            Classes::hkbStateMachineEventPropertyArray(class) => class.name(),
            Classes::hkbStateMachineInternalState(class) => class.name(),
            Classes::hkbStateMachineNestedStateMachineData(class) => class.name(),
            Classes::hkbStateMachineProspectiveTransitionInfo(class) => class.name(),
            Classes::hkbStateMachineStateInfo(class) => class.name(),
            Classes::hkbStateMachineTimeInterval(class) => class.name(),
            Classes::hkbStateMachineTransitionInfo(class) => class.name(),
            Classes::hkbStateMachineTransitionInfoArray(class) => class.name(),
            Classes::hkbStateMachineTransitionInfoReference(class) => class.name(),
            Classes::hkbStringCondition(class) => class.name(),
            Classes::hkbStringEventPayload(class) => class.name(),
            Classes::hkbTestStateChooser(class) => class.name(),
            Classes::hkbTimerModifier(class) => class.name(),
            Classes::hkbTimerModifierInternalState(class) => class.name(),
            Classes::hkbTransformVectorModifier(class) => class.name(),
            Classes::hkbTransformVectorModifierInternalState(class) => class.name(),
            Classes::hkbTransitionEffect(class) => class.name(),
            Classes::hkbTwistModifier(class) => class.name(),
            Classes::hkbVariableBindingSet(class) => class.name(),
            Classes::hkbVariableBindingSetBinding(class) => class.name(),
            Classes::hkbVariableInfo(class) => class.name(),
            Classes::hkbVariableValue(class) => class.name(),
            Classes::hkbVariableValueSet(class) => class.name(),
            Classes::hkbWorldEnums(class) => class.name(),
            Classes::hkbWorldFromModelModeData(class) => class.name(),
            Classes::hkClass(class) => class.name(),
            Classes::hkClassEnum(class) => class.name(),
            Classes::hkClassEnumItem(class) => class.name(),
            Classes::hkClassMember(class) => class.name(),
            Classes::hkColor(class) => class.name(),
            Classes::hkContactPoint(class) => class.name(),
            Classes::hkContactPointMaterial(class) => class.name(),
            Classes::hkCustomAttributes(class) => class.name(),
            Classes::hkCustomAttributesAttribute(class) => class.name(),
            Classes::hkDataObjectTypeAttribute(class) => class.name(),
            Classes::hkDescriptionAttribute(class) => class.name(),
            Classes::hkDocumentationAttribute(class) => class.name(),
            Classes::hkGeometry(class) => class.name(),
            Classes::hkGeometryTriangle(class) => class.name(),
            Classes::hkGizmoAttribute(class) => class.name(),
            Classes::hkHalf8(class) => class.name(),
            Classes::hkIndexedTransformSet(class) => class.name(),
            Classes::hkLinkAttribute(class) => class.name(),
            Classes::hkLocalFrame(class) => class.name(),
            Classes::hkLocalFrameGroup(class) => class.name(),
            Classes::hkMemoryMeshBody(class) => class.name(),
            Classes::hkMemoryMeshMaterial(class) => class.name(),
            Classes::hkMemoryMeshShape(class) => class.name(),
            Classes::hkMemoryMeshTexture(class) => class.name(),
            Classes::hkMemoryMeshVertexBuffer(class) => class.name(),
            Classes::hkMemoryResourceContainer(class) => class.name(),
            Classes::hkMemoryResourceHandle(class) => class.name(),
            Classes::hkMemoryResourceHandleExternalLink(class) => class.name(),
            Classes::hkMemoryTrackerAttribute(class) => class.name(),
            Classes::hkMeshBody(class) => class.name(),
            Classes::hkMeshBoneIndexMapping(class) => class.name(),
            Classes::hkMeshMaterial(class) => class.name(),
            Classes::hkMeshSection(class) => class.name(),
            Classes::hkMeshSectionCinfo(class) => class.name(),
            Classes::hkMeshShape(class) => class.name(),
            Classes::hkMeshTexture(class) => class.name(),
            Classes::hkMeshVertexBuffer(class) => class.name(),
            Classes::hkModelerNodeTypeAttribute(class) => class.name(),
            Classes::hkMonitorStreamColorTable(class) => class.name(),
            Classes::hkMonitorStreamColorTableColorPair(class) => class.name(),
            Classes::hkMonitorStreamFrameInfo(class) => class.name(),
            Classes::hkMonitorStreamStringMap(class) => class.name(),
            Classes::hkMonitorStreamStringMapStringMap(class) => class.name(),
            Classes::hkMoppBvTreeShapeBase(class) => class.name(),
            Classes::hkMotionState(class) => class.name(),
            Classes::hkMultipleVertexBuffer(class) => class.name(),
            Classes::hkMultipleVertexBufferElementInfo(class) => class.name(),
            Classes::hkMultipleVertexBufferLockedElement(class) => class.name(),
            Classes::hkMultipleVertexBufferVertexBufferInfo(class) => class.name(),
            Classes::hkMultiThreadCheck(class) => class.name(),
            Classes::hkp2dAngConstraintAtom(class) => class.name(),
            Classes::hkpAabbPhantom(class) => class.name(),
            Classes::hkPackedVector3(class) => class.name(),
            Classes::hkPackfileHeader(class) => class.name(),
            Classes::hkPackfileSectionHeader(class) => class.name(),
            Classes::hkpAction(class) => class.name(),
            Classes::hkpAgent1nSector(class) => class.name(),
            Classes::hkpAngConstraintAtom(class) => class.name(),
            Classes::hkpAngFrictionConstraintAtom(class) => class.name(),
            Classes::hkpAngLimitConstraintAtom(class) => class.name(),
            Classes::hkpAngMotorConstraintAtom(class) => class.name(),
            Classes::hkpAngularDashpotAction(class) => class.name(),
            Classes::hkpArrayAction(class) => class.name(),
            Classes::hkpBallAndSocketConstraintData(class) => class.name(),
            Classes::hkpBallAndSocketConstraintDataAtoms(class) => class.name(),
            Classes::hkpBallGun(class) => class.name(),
            Classes::hkpBallSocketChainData(class) => class.name(),
            Classes::hkpBallSocketChainDataConstraintInfo(class) => class.name(),
            Classes::hkpBallSocketConstraintAtom(class) => class.name(),
            Classes::hkpBinaryAction(class) => class.name(),
            Classes::hkpBoxMotion(class) => class.name(),
            Classes::hkpBoxShape(class) => class.name(),
            Classes::hkpBreakableBody(class) => class.name(),
            Classes::hkpBreakableConstraintData(class) => class.name(),
            Classes::hkpBridgeAtoms(class) => class.name(),
            Classes::hkpBridgeConstraintAtom(class) => class.name(),
            Classes::hkpBroadPhaseHandle(class) => class.name(),
            Classes::hkpBvShape(class) => class.name(),
            Classes::hkpBvTreeShape(class) => class.name(),
            Classes::hkpCachingShapePhantom(class) => class.name(),
            Classes::hkpCallbackConstraintMotor(class) => class.name(),
            Classes::hkpCapsuleShape(class) => class.name(),
            Classes::hkpCdBody(class) => class.name(),
            Classes::hkpCenterOfMassChangerModifierConstraintAtom(class) => class.name(),
            Classes::hkpCharacterControllerCinfo(class) => class.name(),
            Classes::hkpCharacterMotion(class) => class.name(),
            Classes::hkpCharacterProxyCinfo(class) => class.name(),
            Classes::hkpCharacterRigidBodyCinfo(class) => class.name(),
            Classes::hkpCogWheelConstraintAtom(class) => class.name(),
            Classes::hkpCogWheelConstraintData(class) => class.name(),
            Classes::hkpCogWheelConstraintDataAtoms(class) => class.name(),
            Classes::hkpCollidable(class) => class.name(),
            Classes::hkpCollidableBoundingVolumeData(class) => class.name(),
            Classes::hkpCollidableCollidableFilter(class) => class.name(),
            Classes::hkpCollisionFilter(class) => class.name(),
            Classes::hkpCollisionFilterList(class) => class.name(),
            Classes::hkpCompressedMeshShape(class) => class.name(),
            Classes::hkpCompressedMeshShapeBigTriangle(class) => class.name(),
            Classes::hkpCompressedMeshShapeChunk(class) => class.name(),
            Classes::hkpCompressedMeshShapeConvexPiece(class) => class.name(),
            Classes::hkpCompressedSampledHeightFieldShape(class) => class.name(),
            Classes::hkpConeLimitConstraintAtom(class) => class.name(),
            Classes::hkpConstrainedSystemFilter(class) => class.name(),
            Classes::hkpConstraintAtom(class) => class.name(),
            Classes::hkpConstraintChainData(class) => class.name(),
            Classes::hkpConstraintChainInstance(class) => class.name(),
            Classes::hkpConstraintChainInstanceAction(class) => class.name(),
            Classes::hkpConstraintCollisionFilter(class) => class.name(),
            Classes::hkpConstraintData(class) => class.name(),
            Classes::hkpConstraintInstance(class) => class.name(),
            Classes::hkpConstraintInstanceSmallArraySerializeOverrideType(class) => {
                class.name()
            }
            Classes::hkpConstraintMotor(class) => class.name(),
            Classes::hkpConvexListFilter(class) => class.name(),
            Classes::hkpConvexListShape(class) => class.name(),
            Classes::hkpConvexPieceMeshShape(class) => class.name(),
            Classes::hkpConvexPieceStreamData(class) => class.name(),
            Classes::hkpConvexShape(class) => class.name(),
            Classes::hkpConvexTransformShape(class) => class.name(),
            Classes::hkpConvexTransformShapeBase(class) => class.name(),
            Classes::hkpConvexTranslateShape(class) => class.name(),
            Classes::hkpConvexVerticesConnectivity(class) => class.name(),
            Classes::hkpConvexVerticesShape(class) => class.name(),
            Classes::hkpConvexVerticesShapeFourVectors(class) => class.name(),
            Classes::hkpCylinderShape(class) => class.name(),
            Classes::hkpDashpotAction(class) => class.name(),
            Classes::hkpDefaultConvexListFilter(class) => class.name(),
            Classes::hkpDefaultWorldMemoryWatchDog(class) => class.name(),
            Classes::hkpDisableEntityCollisionFilter(class) => class.name(),
            Classes::hkpDisplayBindingData(class) => class.name(),
            Classes::hkpDisplayBindingDataPhysicsSystem(class) => class.name(),
            Classes::hkpDisplayBindingDataRigidBody(class) => class.name(),
            Classes::hkpEntity(class) => class.name(),
            Classes::hkpEntityExtendedListeners(class) => class.name(),
            Classes::hkpEntitySmallArraySerializeOverrideType(class) => class.name(),
            Classes::hkpEntitySpuCollisionCallback(class) => class.name(),
            Classes::hkpExtendedMeshShape(class) => class.name(),
            Classes::hkpExtendedMeshShapeShapesSubpart(class) => class.name(),
            Classes::hkpExtendedMeshShapeSubpart(class) => class.name(),
            Classes::hkpExtendedMeshShapeTrianglesSubpart(class) => class.name(),
            Classes::hkpFastMeshShape(class) => class.name(),
            Classes::hkpFirstPersonGun(class) => class.name(),
            Classes::hkpFixedRigidMotion(class) => class.name(),
            Classes::hkpGenericConstraintData(class) => class.name(),
            Classes::hkpGenericConstraintDataScheme(class) => class.name(),
            Classes::hkpGenericConstraintDataSchemeConstraintInfo(class) => class.name(),
            Classes::hkpGravityGun(class) => class.name(),
            Classes::hkpGroupCollisionFilter(class) => class.name(),
            Classes::hkpGroupFilter(class) => class.name(),
            Classes::hkpHeightFieldShape(class) => class.name(),
            Classes::hkpHingeConstraintData(class) => class.name(),
            Classes::hkpHingeConstraintDataAtoms(class) => class.name(),
            Classes::hkpHingeLimitsData(class) => class.name(),
            Classes::hkpHingeLimitsDataAtoms(class) => class.name(),
            Classes::hkpIgnoreModifierConstraintAtom(class) => class.name(),
            Classes::hkpKeyframedRigidMotion(class) => class.name(),
            Classes::hkpLimitedForceConstraintMotor(class) => class.name(),
            Classes::hkpLimitedHingeConstraintData(class) => class.name(),
            Classes::hkpLimitedHingeConstraintDataAtoms(class) => class.name(),
            Classes::hkpLinConstraintAtom(class) => class.name(),
            Classes::hkpLinearParametricCurve(class) => class.name(),
            Classes::hkpLinFrictionConstraintAtom(class) => class.name(),
            Classes::hkpLinkedCollidable(class) => class.name(),
            Classes::hkpLinLimitConstraintAtom(class) => class.name(),
            Classes::hkpLinMotorConstraintAtom(class) => class.name(),
            Classes::hkpLinSoftConstraintAtom(class) => class.name(),
            Classes::hkpListShape(class) => class.name(),
            Classes::hkpListShapeChildInfo(class) => class.name(),
            Classes::hkpMalleableConstraintData(class) => class.name(),
            Classes::hkpMassChangerModifierConstraintAtom(class) => class.name(),
            Classes::hkpMassProperties(class) => class.name(),
            Classes::hkpMaterial(class) => class.name(),
            Classes::hkpMaxSizeMotion(class) => class.name(),
            Classes::hkpMeshMaterial(class) => class.name(),
            Classes::hkpMeshShape(class) => class.name(),
            Classes::hkpMeshShapeSubpart(class) => class.name(),
            Classes::hkpModifierConstraintAtom(class) => class.name(),
            Classes::hkpMoppBvTreeShape(class) => class.name(),
            Classes::hkpMoppCode(class) => class.name(),
            Classes::hkpMoppCodeCodeInfo(class) => class.name(),
            Classes::hkpMoppCodeReindexedTerminal(class) => class.name(),
            Classes::hkpMotion(class) => class.name(),
            Classes::hkpMotorAction(class) => class.name(),
            Classes::hkpMountedBallGun(class) => class.name(),
            Classes::hkpMouseSpringAction(class) => class.name(),
            Classes::hkpMovingSurfaceModifierConstraintAtom(class) => class.name(),
            Classes::hkpMultiRayShape(class) => class.name(),
            Classes::hkpMultiRayShapeRay(class) => class.name(),
            Classes::hkpMultiSphereShape(class) => class.name(),
            Classes::hkpMultithreadedVehicleManager(class) => class.name(),
            Classes::hkpNamedMeshMaterial(class) => class.name(),
            Classes::hkpNullCollisionFilter(class) => class.name(),
            Classes::hkPostFinishAttribute(class) => class.name(),
            Classes::hkpOverwritePivotConstraintAtom(class) => class.name(),
            Classes::hkpPairCollisionFilter(class) => class.name(),
            Classes::hkpPairCollisionFilterMapPairFilterKeyOverrideType(class) => {
                class.name()
            }
            Classes::hkpParametricCurve(class) => class.name(),
            Classes::hkpPhantom(class) => class.name(),
            Classes::hkpPhantomCallbackShape(class) => class.name(),
            Classes::hkpPhysicsData(class) => class.name(),
            Classes::hkpPhysicsSystem(class) => class.name(),
            Classes::hkpPhysicsSystemWithContacts(class) => class.name(),
            Classes::hkpPlaneShape(class) => class.name(),
            Classes::hkpPointToPathConstraintData(class) => class.name(),
            Classes::hkpPointToPlaneConstraintData(class) => class.name(),
            Classes::hkpPointToPlaneConstraintDataAtoms(class) => class.name(),
            Classes::hkpPositionConstraintMotor(class) => class.name(),
            Classes::hkpPoweredChainData(class) => class.name(),
            Classes::hkpPoweredChainDataConstraintInfo(class) => class.name(),
            Classes::hkpPoweredChainMapper(class) => class.name(),
            Classes::hkpPoweredChainMapperLinkInfo(class) => class.name(),
            Classes::hkpPoweredChainMapperTarget(class) => class.name(),
            Classes::hkpPrismaticConstraintData(class) => class.name(),
            Classes::hkpPrismaticConstraintDataAtoms(class) => class.name(),
            Classes::hkpProjectileGun(class) => class.name(),
            Classes::hkpProperty(class) => class.name(),
            Classes::hkpPropertyValue(class) => class.name(),
            Classes::hkpPulleyConstraintAtom(class) => class.name(),
            Classes::hkpPulleyConstraintData(class) => class.name(),
            Classes::hkpPulleyConstraintDataAtoms(class) => class.name(),
            Classes::hkpRackAndPinionConstraintAtom(class) => class.name(),
            Classes::hkpRackAndPinionConstraintData(class) => class.name(),
            Classes::hkpRackAndPinionConstraintDataAtoms(class) => class.name(),
            Classes::hkpRagdollConstraintData(class) => class.name(),
            Classes::hkpRagdollConstraintDataAtoms(class) => class.name(),
            Classes::hkpRagdollLimitsData(class) => class.name(),
            Classes::hkpRagdollLimitsDataAtoms(class) => class.name(),
            Classes::hkpRagdollMotorConstraintAtom(class) => class.name(),
            Classes::hkpRayCollidableFilter(class) => class.name(),
            Classes::hkpRayShapeCollectionFilter(class) => class.name(),
            Classes::hkpRejectChassisListener(class) => class.name(),
            Classes::hkpRemoveTerminalsMoppModifier(class) => class.name(),
            Classes::hkpReorientAction(class) => class.name(),
            Classes::hkpRigidBody(class) => class.name(),
            Classes::hkpRotationalConstraintData(class) => class.name(),
            Classes::hkpRotationalConstraintDataAtoms(class) => class.name(),
            Classes::hkpSampledHeightFieldShape(class) => class.name(),
            Classes::hkpSerializedAgentNnEntry(class) => class.name(),
            Classes::hkpSerializedDisplayMarker(class) => class.name(),
            Classes::hkpSerializedDisplayMarkerList(class) => class.name(),
            Classes::hkpSerializedDisplayRbTransforms(class) => class.name(),
            Classes::hkpSerializedDisplayRbTransformsDisplayTransformPair(class) => {
                class.name()
            }
            Classes::hkpSerializedSubTrack1nInfo(class) => class.name(),
            Classes::hkpSerializedTrack1nInfo(class) => class.name(),
            Classes::hkpSetLocalRotationsConstraintAtom(class) => class.name(),
            Classes::hkpSetLocalTransformsConstraintAtom(class) => class.name(),
            Classes::hkpSetLocalTranslationsConstraintAtom(class) => class.name(),
            Classes::hkpSetupStabilizationAtom(class) => class.name(),
            Classes::hkpShape(class) => class.name(),
            Classes::hkpShapeCollection(class) => class.name(),
            Classes::hkpShapeCollectionFilter(class) => class.name(),
            Classes::hkpShapeContainer(class) => class.name(),
            Classes::hkpShapeInfo(class) => class.name(),
            Classes::hkpShapeModifier(class) => class.name(),
            Classes::hkpShapePhantom(class) => class.name(),
            Classes::hkpSimpleContactConstraintAtom(class) => class.name(),
            Classes::hkpSimpleContactConstraintDataInfo(class) => class.name(),
            Classes::hkpSimpleMeshShape(class) => class.name(),
            Classes::hkpSimpleMeshShapeTriangle(class) => class.name(),
            Classes::hkpSimpleShapePhantom(class) => class.name(),
            Classes::hkpSimpleShapePhantomCollisionDetail(class) => class.name(),
            Classes::hkpSimulation(class) => class.name(),
            Classes::hkpSingleShapeContainer(class) => class.name(),
            Classes::hkpSoftContactModifierConstraintAtom(class) => class.name(),
            Classes::hkpSphereMotion(class) => class.name(),
            Classes::hkpSphereRepShape(class) => class.name(),
            Classes::hkpSphereShape(class) => class.name(),
            Classes::hkpSpringAction(class) => class.name(),
            Classes::hkpSpringDamperConstraintMotor(class) => class.name(),
            Classes::hkpStiffSpringChainData(class) => class.name(),
            Classes::hkpStiffSpringChainDataConstraintInfo(class) => class.name(),
            Classes::hkpStiffSpringConstraintAtom(class) => class.name(),
            Classes::hkpStiffSpringConstraintData(class) => class.name(),
            Classes::hkpStiffSpringConstraintDataAtoms(class) => class.name(),
            Classes::hkpStorageExtendedMeshShape(class) => class.name(),
            Classes::hkpStorageExtendedMeshShapeMaterial(class) => class.name(),
            Classes::hkpStorageExtendedMeshShapeMeshSubpartStorage(class) => class.name(),
            Classes::hkpStorageExtendedMeshShapeShapeSubpartStorage(class) => {
                class.name()
            }
            Classes::hkpStorageMeshShape(class) => class.name(),
            Classes::hkpStorageMeshShapeSubpartStorage(class) => class.name(),
            Classes::hkpStorageSampledHeightFieldShape(class) => class.name(),
            Classes::hkpThinBoxMotion(class) => class.name(),
            Classes::hkpTransformShape(class) => class.name(),
            Classes::hkpTriangleShape(class) => class.name(),
            Classes::hkpTriggerVolume(class) => class.name(),
            Classes::hkpTriggerVolumeEventInfo(class) => class.name(),
            Classes::hkpTriSampledHeightFieldBvTreeShape(class) => class.name(),
            Classes::hkpTriSampledHeightFieldCollection(class) => class.name(),
            Classes::hkpTwistLimitConstraintAtom(class) => class.name(),
            Classes::hkpTypedBroadPhaseHandle(class) => class.name(),
            Classes::hkpTyremarkPoint(class) => class.name(),
            Classes::hkpTyremarksInfo(class) => class.name(),
            Classes::hkpTyremarksWheel(class) => class.name(),
            Classes::hkpUnaryAction(class) => class.name(),
            Classes::hkpVehicleAerodynamics(class) => class.name(),
            Classes::hkpVehicleBrake(class) => class.name(),
            Classes::hkpVehicleCastBatchingManager(class) => class.name(),
            Classes::hkpVehicleData(class) => class.name(),
            Classes::hkpVehicleDataWheelComponentParams(class) => class.name(),
            Classes::hkpVehicleDefaultAerodynamics(class) => class.name(),
            Classes::hkpVehicleDefaultAnalogDriverInput(class) => class.name(),
            Classes::hkpVehicleDefaultBrake(class) => class.name(),
            Classes::hkpVehicleDefaultBrakeWheelBrakingProperties(class) => class.name(),
            Classes::hkpVehicleDefaultEngine(class) => class.name(),
            Classes::hkpVehicleDefaultSteering(class) => class.name(),
            Classes::hkpVehicleDefaultSuspension(class) => class.name(),
            Classes::hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters(
                class,
            ) => class.name(),
            Classes::hkpVehicleDefaultTransmission(class) => class.name(),
            Classes::hkpVehicleDefaultVelocityDamper(class) => class.name(),
            Classes::hkpVehicleDriverInput(class) => class.name(),
            Classes::hkpVehicleDriverInputAnalogStatus(class) => class.name(),
            Classes::hkpVehicleDriverInputStatus(class) => class.name(),
            Classes::hkpVehicleEngine(class) => class.name(),
            Classes::hkpVehicleFrictionDescription(class) => class.name(),
            Classes::hkpVehicleFrictionDescriptionAxisDescription(class) => class.name(),
            Classes::hkpVehicleFrictionStatus(class) => class.name(),
            Classes::hkpVehicleFrictionStatusAxisStatus(class) => class.name(),
            Classes::hkpVehicleInstance(class) => class.name(),
            Classes::hkpVehicleInstanceWheelInfo(class) => class.name(),
            Classes::hkpVehicleLinearCastBatchingManager(class) => class.name(),
            Classes::hkpVehicleLinearCastWheelCollide(class) => class.name(),
            Classes::hkpVehicleLinearCastWheelCollideWheelState(class) => class.name(),
            Classes::hkpVehicleManager(class) => class.name(),
            Classes::hkpVehicleRayCastBatchingManager(class) => class.name(),
            Classes::hkpVehicleRayCastWheelCollide(class) => class.name(),
            Classes::hkpVehicleSteering(class) => class.name(),
            Classes::hkpVehicleSuspension(class) => class.name(),
            Classes::hkpVehicleSuspensionSuspensionWheelParameters(class) => class.name(),
            Classes::hkpVehicleTransmission(class) => class.name(),
            Classes::hkpVehicleVelocityDamper(class) => class.name(),
            Classes::hkpVehicleWheelCollide(class) => class.name(),
            Classes::hkpVelocityConstraintMotor(class) => class.name(),
            Classes::hkpViscousSurfaceModifierConstraintAtom(class) => class.name(),
            Classes::hkpWeldingUtility(class) => class.name(),
            Classes::hkpWheelConstraintData(class) => class.name(),
            Classes::hkpWheelConstraintDataAtoms(class) => class.name(),
            Classes::hkpWorld(class) => class.name(),
            Classes::hkpWorldCinfo(class) => class.name(),
            Classes::hkpWorldObject(class) => class.name(),
            Classes::hkQTransform(class) => class.name(),
            Classes::hkRangeInt32Attribute(class) => class.name(),
            Classes::hkRangeRealAttribute(class) => class.name(),
            Classes::hkReferencedObject(class) => class.name(),
            Classes::hkReflectedFileAttribute(class) => class.name(),
            Classes::hkResourceBase(class) => class.name(),
            Classes::hkResourceContainer(class) => class.name(),
            Classes::hkResourceHandle(class) => class.name(),
            Classes::hkRootLevelContainer(class) => class.name(),
            Classes::hkRootLevelContainerNamedVariant(class) => class.name(),
            Classes::hkSemanticsAttribute(class) => class.name(),
            Classes::hkSimpleLocalFrame(class) => class.name(),
            Classes::hkSphere(class) => class.name(),
            Classes::hkSweptTransform(class) => class.name(),
            Classes::hkTraceStreamTitle(class) => class.name(),
            Classes::hkTrackerSerializableScanSnapshot(class) => class.name(),
            Classes::hkTrackerSerializableScanSnapshotAllocation(class) => class.name(),
            Classes::hkTrackerSerializableScanSnapshotBlock(class) => class.name(),
            Classes::hkUiAttribute(class) => class.name(),
            Classes::hkVertexFormat(class) => class.name(),
            Classes::hkVertexFormatElement(class) => class.name(),
            Classes::hkWorldMemoryAvailableWatchDog(class) => class.name(),
            Classes::hkxAnimatedFloat(class) => class.name(),
            Classes::hkxAnimatedMatrix(class) => class.name(),
            Classes::hkxAnimatedQuaternion(class) => class.name(),
            Classes::hkxAnimatedVector(class) => class.name(),
            Classes::hkxAttribute(class) => class.name(),
            Classes::hkxAttributeGroup(class) => class.name(),
            Classes::hkxAttributeHolder(class) => class.name(),
            Classes::hkxCamera(class) => class.name(),
            Classes::hkxEdgeSelectionChannel(class) => class.name(),
            Classes::hkxEnum(class) => class.name(),
            Classes::hkxEnumItem(class) => class.name(),
            Classes::hkxEnvironment(class) => class.name(),
            Classes::hkxEnvironmentVariable(class) => class.name(),
            Classes::hkxIndexBuffer(class) => class.name(),
            Classes::hkxLight(class) => class.name(),
            Classes::hkxMaterial(class) => class.name(),
            Classes::hkxMaterialEffect(class) => class.name(),
            Classes::hkxMaterialProperty(class) => class.name(),
            Classes::hkxMaterialShader(class) => class.name(),
            Classes::hkxMaterialShaderSet(class) => class.name(),
            Classes::hkxMaterialTextureStage(class) => class.name(),
            Classes::hkxMesh(class) => class.name(),
            Classes::hkxMeshSection(class) => class.name(),
            Classes::hkxMeshUserChannelInfo(class) => class.name(),
            Classes::hkxNode(class) => class.name(),
            Classes::hkxNodeAnnotationData(class) => class.name(),
            Classes::hkxNodeSelectionSet(class) => class.name(),
            Classes::hkxScene(class) => class.name(),
            Classes::hkxSkinBinding(class) => class.name(),
            Classes::hkxSparselyAnimatedBool(class) => class.name(),
            Classes::hkxSparselyAnimatedEnum(class) => class.name(),
            Classes::hkxSparselyAnimatedInt(class) => class.name(),
            Classes::hkxSparselyAnimatedString(class) => class.name(),
            Classes::hkxTextureFile(class) => class.name(),
            Classes::hkxTextureInplace(class) => class.name(),
            Classes::hkxTriangleSelectionChannel(class) => class.name(),
            Classes::hkxVertexBuffer(class) => class.name(),
            Classes::hkxVertexBufferVertexData(class) => class.name(),
            Classes::hkxVertexDescription(class) => class.name(),
            Classes::hkxVertexDescriptionElementDecl(class) => class.name(),
            Classes::hkxVertexFloatDataChannel(class) => class.name(),
            Classes::hkxVertexIntDataChannel(class) => class.name(),
            Classes::hkxVertexSelectionChannel(class) => class.name(),
            Classes::hkxVertexVectorDataChannel(class) => class.name(),
        }
    }
    fn signature(&self) -> _serde::__private::Signature {
        match &self {
            Classes::BGSGamebryoSequenceGenerator(class) => class.signature(),
            Classes::BSBoneSwitchGenerator(class) => class.signature(),
            Classes::BSBoneSwitchGeneratorBoneData(class) => class.signature(),
            Classes::BSComputeAddBoneAnimModifier(class) => class.signature(),
            Classes::BSCyclicBlendTransitionGenerator(class) => class.signature(),
            Classes::BSDecomposeVectorModifier(class) => class.signature(),
            Classes::BSDirectAtModifier(class) => class.signature(),
            Classes::BSDistTriggerModifier(class) => class.signature(),
            Classes::BSEventEveryNEventsModifier(class) => class.signature(),
            Classes::BSEventOnDeactivateModifier(class) => class.signature(),
            Classes::BSEventOnFalseToTrueModifier(class) => class.signature(),
            Classes::BSGetTimeStepModifier(class) => class.signature(),
            Classes::BSInterpValueModifier(class) => class.signature(),
            Classes::BSIsActiveModifier(class) => class.signature(),
            Classes::BSIStateManagerModifier(class) => class.signature(),
            Classes::BSIStateManagerModifierBSiStateData(class) => class.signature(),
            Classes::BSIStateManagerModifierBSIStateManagerStateListener(class) => {
                class.signature()
            }
            Classes::BSiStateTaggingGenerator(class) => class.signature(),
            Classes::BSLimbIKModifier(class) => class.signature(),
            Classes::BSLookAtModifier(class) => class.signature(),
            Classes::BSLookAtModifierBoneData(class) => class.signature(),
            Classes::BSModifyOnceModifier(class) => class.signature(),
            Classes::BSOffsetAnimationGenerator(class) => class.signature(),
            Classes::BSPassByTargetTriggerModifier(class) => class.signature(),
            Classes::BSRagdollContactListenerModifier(class) => class.signature(),
            Classes::BSSpeedSamplerModifier(class) => class.signature(),
            Classes::BSSynchronizedClipGenerator(class) => class.signature(),
            Classes::BSTimerModifier(class) => class.signature(),
            Classes::BSTweenerModifier(class) => class.signature(),
            Classes::hkAabb(class) => class.signature(),
            Classes::hkAabbHalf(class) => class.signature(),
            Classes::hkAabbUint32(class) => class.signature(),
            Classes::hkaAnimatedReferenceFrame(class) => class.signature(),
            Classes::hkaAnimation(class) => class.signature(),
            Classes::hkaAnimationBinding(class) => class.signature(),
            Classes::hkaAnimationContainer(class) => class.signature(),
            Classes::hkaAnimationPreviewColorContainer(class) => class.signature(),
            Classes::hkaAnnotationTrack(class) => class.signature(),
            Classes::hkaAnnotationTrackAnnotation(class) => class.signature(),
            Classes::hkaBone(class) => class.signature(),
            Classes::hkaBoneAttachment(class) => class.signature(),
            Classes::hkaDefaultAnimatedReferenceFrame(class) => class.signature(),
            Classes::hkaDeltaCompressedAnimation(class) => class.signature(),
            Classes::hkaDeltaCompressedAnimationQuantizationFormat(class) => {
                class.signature()
            }
            Classes::hkaFootstepAnalysisInfo(class) => class.signature(),
            Classes::hkaFootstepAnalysisInfoContainer(class) => class.signature(),
            Classes::hkaInterleavedUncompressedAnimation(class) => class.signature(),
            Classes::hkaKeyFrameHierarchyUtility(class) => class.signature(),
            Classes::hkaKeyFrameHierarchyUtilityControlData(class) => class.signature(),
            Classes::hkAlignSceneToNodeOptions(class) => class.signature(),
            Classes::hkaMeshBinding(class) => class.signature(),
            Classes::hkaMeshBindingMapping(class) => class.signature(),
            Classes::hkaQuantizedAnimation(class) => class.signature(),
            Classes::hkaQuantizedAnimationTrackCompressionParams(class) => {
                class.signature()
            }
            Classes::hkaRagdollInstance(class) => class.signature(),
            Classes::hkArrayTypeAttribute(class) => class.signature(),
            Classes::hkaSkeleton(class) => class.signature(),
            Classes::hkaSkeletonLocalFrameOnBone(class) => class.signature(),
            Classes::hkaSkeletonMapper(class) => class.signature(),
            Classes::hkaSkeletonMapperData(class) => class.signature(),
            Classes::hkaSkeletonMapperDataChainMapping(class) => class.signature(),
            Classes::hkaSkeletonMapperDataSimpleMapping(class) => class.signature(),
            Classes::hkaSplineCompressedAnimation(class) => class.signature(),
            Classes::hkaSplineCompressedAnimationAnimationCompressionParams(class) => {
                class.signature()
            }
            Classes::hkaSplineCompressedAnimationTrackCompressionParams(class) => {
                class.signature()
            }
            Classes::hkaWaveletCompressedAnimation(class) => class.signature(),
            Classes::hkaWaveletCompressedAnimationCompressionParams(class) => {
                class.signature()
            }
            Classes::hkaWaveletCompressedAnimationQuantizationFormat(class) => {
                class.signature()
            }
            Classes::hkBaseObject(class) => class.signature(),
            Classes::hkbAttachmentModifier(class) => class.signature(),
            Classes::hkbAttachmentSetup(class) => class.signature(),
            Classes::hkbAttributeModifier(class) => class.signature(),
            Classes::hkbAttributeModifierAssignment(class) => class.signature(),
            Classes::hkbAuxiliaryNodeInfo(class) => class.signature(),
            Classes::hkbBehaviorEventsInfo(class) => class.signature(),
            Classes::hkbBehaviorGraph(class) => class.signature(),
            Classes::hkbBehaviorGraphData(class) => class.signature(),
            Classes::hkbBehaviorGraphInternalState(class) => class.signature(),
            Classes::hkbBehaviorGraphInternalStateInfo(class) => class.signature(),
            Classes::hkbBehaviorGraphStringData(class) => class.signature(),
            Classes::hkbBehaviorInfo(class) => class.signature(),
            Classes::hkbBehaviorInfoIdToNamePair(class) => class.signature(),
            Classes::hkbBehaviorReferenceGenerator(class) => class.signature(),
            Classes::hkbBindable(class) => class.signature(),
            Classes::hkbBlendCurveUtils(class) => class.signature(),
            Classes::hkbBlenderGenerator(class) => class.signature(),
            Classes::hkbBlenderGeneratorChild(class) => class.signature(),
            Classes::hkbBlenderGeneratorChildInternalState(class) => class.signature(),
            Classes::hkbBlenderGeneratorInternalState(class) => class.signature(),
            Classes::hkbBlendingTransitionEffect(class) => class.signature(),
            Classes::hkbBlendingTransitionEffectInternalState(class) => class.signature(),
            Classes::hkbBoneIndexArray(class) => class.signature(),
            Classes::hkbBoneWeightArray(class) => class.signature(),
            Classes::hkbBoolVariableSequencedData(class) => class.signature(),
            Classes::hkbBoolVariableSequencedDataSample(class) => class.signature(),
            Classes::hkbCameraShakeEventPayload(class) => class.signature(),
            Classes::hkbCharacter(class) => class.signature(),
            Classes::hkbCharacterAddedInfo(class) => class.signature(),
            Classes::hkbCharacterControlCommand(class) => class.signature(),
            Classes::hkbCharacterControllerControlData(class) => class.signature(),
            Classes::hkbCharacterControllerModifier(class) => class.signature(),
            Classes::hkbCharacterControllerModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbCharacterData(class) => class.signature(),
            Classes::hkbCharacterDataCharacterControllerInfo(class) => class.signature(),
            Classes::hkbCharacterInfo(class) => class.signature(),
            Classes::hkbCharacterSetup(class) => class.signature(),
            Classes::hkbCharacterSkinInfo(class) => class.signature(),
            Classes::hkbCharacterSteppedInfo(class) => class.signature(),
            Classes::hkbCharacterStringData(class) => class.signature(),
            Classes::hkbClientCharacterState(class) => class.signature(),
            Classes::hkbClipGenerator(class) => class.signature(),
            Classes::hkbClipGeneratorEcho(class) => class.signature(),
            Classes::hkbClipGeneratorInternalState(class) => class.signature(),
            Classes::hkbClipTrigger(class) => class.signature(),
            Classes::hkbClipTriggerArray(class) => class.signature(),
            Classes::hkbCombineTransformsModifier(class) => class.signature(),
            Classes::hkbCombineTransformsModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbCompiledExpressionSet(class) => class.signature(),
            Classes::hkbCompiledExpressionSetToken(class) => class.signature(),
            Classes::hkbComputeDirectionModifier(class) => class.signature(),
            Classes::hkbComputeDirectionModifierInternalState(class) => class.signature(),
            Classes::hkbComputeRotationFromAxisAngleModifier(class) => class.signature(),
            Classes::hkbComputeRotationFromAxisAngleModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbComputeRotationToTargetModifier(class) => class.signature(),
            Classes::hkbComputeRotationToTargetModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbCondition(class) => class.signature(),
            Classes::hkbContext(class) => class.signature(),
            Classes::hkbDampingModifier(class) => class.signature(),
            Classes::hkbDampingModifierInternalState(class) => class.signature(),
            Classes::hkbDefaultMessageLog(class) => class.signature(),
            Classes::hkbDelayedModifier(class) => class.signature(),
            Classes::hkbDelayedModifierInternalState(class) => class.signature(),
            Classes::hkbDetectCloseToGroundModifier(class) => class.signature(),
            Classes::hkbDetectCloseToGroundModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbEvaluateExpressionModifier(class) => class.signature(),
            Classes::hkbEvaluateExpressionModifierInternalExpressionData(class) => {
                class.signature()
            }
            Classes::hkbEvaluateExpressionModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbEvaluateHandleModifier(class) => class.signature(),
            Classes::hkbEvent(class) => class.signature(),
            Classes::hkbEventBase(class) => class.signature(),
            Classes::hkbEventDrivenModifier(class) => class.signature(),
            Classes::hkbEventDrivenModifierInternalState(class) => class.signature(),
            Classes::hkbEventInfo(class) => class.signature(),
            Classes::hkbEventPayload(class) => class.signature(),
            Classes::hkbEventPayloadList(class) => class.signature(),
            Classes::hkbEventProperty(class) => class.signature(),
            Classes::hkbEventRaisedInfo(class) => class.signature(),
            Classes::hkbEventRangeData(class) => class.signature(),
            Classes::hkbEventRangeDataArray(class) => class.signature(),
            Classes::hkbEventSequencedData(class) => class.signature(),
            Classes::hkbEventSequencedDataSequencedEvent(class) => class.signature(),
            Classes::hkbEventsFromRangeModifier(class) => class.signature(),
            Classes::hkbEventsFromRangeModifierInternalState(class) => class.signature(),
            Classes::hkbExpressionCondition(class) => class.signature(),
            Classes::hkbExpressionData(class) => class.signature(),
            Classes::hkbExpressionDataArray(class) => class.signature(),
            Classes::hkbExtractRagdollPoseModifier(class) => class.signature(),
            Classes::hkbFootIkControlData(class) => class.signature(),
            Classes::hkbFootIkControlsModifier(class) => class.signature(),
            Classes::hkbFootIkControlsModifierLeg(class) => class.signature(),
            Classes::hkbFootIkDriverInfo(class) => class.signature(),
            Classes::hkbFootIkDriverInfoLeg(class) => class.signature(),
            Classes::hkbFootIkGains(class) => class.signature(),
            Classes::hkbFootIkModifier(class) => class.signature(),
            Classes::hkbFootIkModifierInternalLegData(class) => class.signature(),
            Classes::hkbFootIkModifierLeg(class) => class.signature(),
            Classes::hkbGenerator(class) => class.signature(),
            Classes::hkbGeneratorOutputListener(class) => class.signature(),
            Classes::hkbGeneratorSyncInfo(class) => class.signature(),
            Classes::hkbGeneratorSyncInfoSyncPoint(class) => class.signature(),
            Classes::hkbGeneratorTransitionEffect(class) => class.signature(),
            Classes::hkbGeneratorTransitionEffectInternalState(class) => {
                class.signature()
            }
            Classes::hkbGetHandleOnBoneModifier(class) => class.signature(),
            Classes::hkbGetUpModifier(class) => class.signature(),
            Classes::hkbGetUpModifierInternalState(class) => class.signature(),
            Classes::hkbGetWorldFromModelModifier(class) => class.signature(),
            Classes::hkbGetWorldFromModelModifierInternalState(class) => {
                class.signature()
            }
            Classes::hkbHandIkControlData(class) => class.signature(),
            Classes::hkbHandIkControlsModifier(class) => class.signature(),
            Classes::hkbHandIkControlsModifierHand(class) => class.signature(),
            Classes::hkbHandIkDriverInfo(class) => class.signature(),
            Classes::hkbHandIkDriverInfoHand(class) => class.signature(),
            Classes::hkbHandIkModifier(class) => class.signature(),
            Classes::hkbHandIkModifierHand(class) => class.signature(),
            Classes::hkbHandle(class) => class.signature(),
            Classes::hkbIntEventPayload(class) => class.signature(),
            Classes::hkbIntVariableSequencedData(class) => class.signature(),
            Classes::hkbIntVariableSequencedDataSample(class) => class.signature(),
            Classes::hkBitField(class) => class.signature(),
            Classes::hkbKeyframeBonesModifier(class) => class.signature(),
            Classes::hkbKeyframeBonesModifierKeyframeInfo(class) => class.signature(),
            Classes::hkbLinkedSymbolInfo(class) => class.signature(),
            Classes::hkbLookAtModifier(class) => class.signature(),
            Classes::hkbLookAtModifierInternalState(class) => class.signature(),
            Classes::hkbManualSelectorGenerator(class) => class.signature(),
            Classes::hkbManualSelectorGeneratorInternalState(class) => class.signature(),
            Classes::hkbMessageLog(class) => class.signature(),
            Classes::hkbMirroredSkeletonInfo(class) => class.signature(),
            Classes::hkbMirrorModifier(class) => class.signature(),
            Classes::hkbModifier(class) => class.signature(),
            Classes::hkbModifierGenerator(class) => class.signature(),
            Classes::hkbModifierList(class) => class.signature(),
            Classes::hkbModifierWrapper(class) => class.signature(),
            Classes::hkbMoveCharacterModifier(class) => class.signature(),
            Classes::hkbMoveCharacterModifierInternalState(class) => class.signature(),
            Classes::hkbNamedEventPayload(class) => class.signature(),
            Classes::hkbNamedIntEventPayload(class) => class.signature(),
            Classes::hkbNamedRealEventPayload(class) => class.signature(),
            Classes::hkbNamedStringEventPayload(class) => class.signature(),
            Classes::hkbNode(class) => class.signature(),
            Classes::hkbNodeInternalStateInfo(class) => class.signature(),
            Classes::hkbParticleSystemEventPayload(class) => class.signature(),
            Classes::hkbPoseMatchingGenerator(class) => class.signature(),
            Classes::hkbPoseMatchingGeneratorInternalState(class) => class.signature(),
            Classes::hkbPoweredRagdollControlData(class) => class.signature(),
            Classes::hkbPoweredRagdollControlsModifier(class) => class.signature(),
            Classes::hkbProjectData(class) => class.signature(),
            Classes::hkbProjectStringData(class) => class.signature(),
            Classes::hkbProxyModifier(class) => class.signature(),
            Classes::hkbProxyModifierProxyInfo(class) => class.signature(),
            Classes::hkbRaiseEventCommand(class) => class.signature(),
            Classes::hkbRealEventPayload(class) => class.signature(),
            Classes::hkbRealVariableSequencedData(class) => class.signature(),
            Classes::hkbRealVariableSequencedDataSample(class) => class.signature(),
            Classes::hkbReferencePoseGenerator(class) => class.signature(),
            Classes::hkbRegisteredGenerator(class) => class.signature(),
            Classes::hkbRigidBodyRagdollControlData(class) => class.signature(),
            Classes::hkbRigidBodyRagdollControlsModifier(class) => class.signature(),
            Classes::hkbRoleAttribute(class) => class.signature(),
            Classes::hkbRotateCharacterModifier(class) => class.signature(),
            Classes::hkbRotateCharacterModifierInternalState(class) => class.signature(),
            Classes::hkbSenseHandleModifier(class) => class.signature(),
            Classes::hkbSenseHandleModifierRange(class) => class.signature(),
            Classes::hkbSequence(class) => class.signature(),
            Classes::hkbSequencedData(class) => class.signature(),
            Classes::hkbSequenceInternalState(class) => class.signature(),
            Classes::hkbSequenceStringData(class) => class.signature(),
            Classes::hkbSetBehaviorCommand(class) => class.signature(),
            Classes::hkbSetLocalTimeOfClipGeneratorCommand(class) => class.signature(),
            Classes::hkbSetNodePropertyCommand(class) => class.signature(),
            Classes::hkbSetWordVariableCommand(class) => class.signature(),
            Classes::hkbSetWorldFromModelModifier(class) => class.signature(),
            Classes::hkbSimulationControlCommand(class) => class.signature(),
            Classes::hkbSimulationStateInfo(class) => class.signature(),
            Classes::hkbStateChooser(class) => class.signature(),
            Classes::hkbStateListener(class) => class.signature(),
            Classes::hkbStateMachine(class) => class.signature(),
            Classes::hkbStateMachineActiveTransitionInfo(class) => class.signature(),
            Classes::hkbStateMachineDelayedTransitionInfo(class) => class.signature(),
            Classes::hkbStateMachineEventPropertyArray(class) => class.signature(),
            Classes::hkbStateMachineInternalState(class) => class.signature(),
            Classes::hkbStateMachineNestedStateMachineData(class) => class.signature(),
            Classes::hkbStateMachineProspectiveTransitionInfo(class) => class.signature(),
            Classes::hkbStateMachineStateInfo(class) => class.signature(),
            Classes::hkbStateMachineTimeInterval(class) => class.signature(),
            Classes::hkbStateMachineTransitionInfo(class) => class.signature(),
            Classes::hkbStateMachineTransitionInfoArray(class) => class.signature(),
            Classes::hkbStateMachineTransitionInfoReference(class) => class.signature(),
            Classes::hkbStringCondition(class) => class.signature(),
            Classes::hkbStringEventPayload(class) => class.signature(),
            Classes::hkbTestStateChooser(class) => class.signature(),
            Classes::hkbTimerModifier(class) => class.signature(),
            Classes::hkbTimerModifierInternalState(class) => class.signature(),
            Classes::hkbTransformVectorModifier(class) => class.signature(),
            Classes::hkbTransformVectorModifierInternalState(class) => class.signature(),
            Classes::hkbTransitionEffect(class) => class.signature(),
            Classes::hkbTwistModifier(class) => class.signature(),
            Classes::hkbVariableBindingSet(class) => class.signature(),
            Classes::hkbVariableBindingSetBinding(class) => class.signature(),
            Classes::hkbVariableInfo(class) => class.signature(),
            Classes::hkbVariableValue(class) => class.signature(),
            Classes::hkbVariableValueSet(class) => class.signature(),
            Classes::hkbWorldEnums(class) => class.signature(),
            Classes::hkbWorldFromModelModeData(class) => class.signature(),
            Classes::hkClass(class) => class.signature(),
            Classes::hkClassEnum(class) => class.signature(),
            Classes::hkClassEnumItem(class) => class.signature(),
            Classes::hkClassMember(class) => class.signature(),
            Classes::hkColor(class) => class.signature(),
            Classes::hkContactPoint(class) => class.signature(),
            Classes::hkContactPointMaterial(class) => class.signature(),
            Classes::hkCustomAttributes(class) => class.signature(),
            Classes::hkCustomAttributesAttribute(class) => class.signature(),
            Classes::hkDataObjectTypeAttribute(class) => class.signature(),
            Classes::hkDescriptionAttribute(class) => class.signature(),
            Classes::hkDocumentationAttribute(class) => class.signature(),
            Classes::hkGeometry(class) => class.signature(),
            Classes::hkGeometryTriangle(class) => class.signature(),
            Classes::hkGizmoAttribute(class) => class.signature(),
            Classes::hkHalf8(class) => class.signature(),
            Classes::hkIndexedTransformSet(class) => class.signature(),
            Classes::hkLinkAttribute(class) => class.signature(),
            Classes::hkLocalFrame(class) => class.signature(),
            Classes::hkLocalFrameGroup(class) => class.signature(),
            Classes::hkMemoryMeshBody(class) => class.signature(),
            Classes::hkMemoryMeshMaterial(class) => class.signature(),
            Classes::hkMemoryMeshShape(class) => class.signature(),
            Classes::hkMemoryMeshTexture(class) => class.signature(),
            Classes::hkMemoryMeshVertexBuffer(class) => class.signature(),
            Classes::hkMemoryResourceContainer(class) => class.signature(),
            Classes::hkMemoryResourceHandle(class) => class.signature(),
            Classes::hkMemoryResourceHandleExternalLink(class) => class.signature(),
            Classes::hkMemoryTrackerAttribute(class) => class.signature(),
            Classes::hkMeshBody(class) => class.signature(),
            Classes::hkMeshBoneIndexMapping(class) => class.signature(),
            Classes::hkMeshMaterial(class) => class.signature(),
            Classes::hkMeshSection(class) => class.signature(),
            Classes::hkMeshSectionCinfo(class) => class.signature(),
            Classes::hkMeshShape(class) => class.signature(),
            Classes::hkMeshTexture(class) => class.signature(),
            Classes::hkMeshVertexBuffer(class) => class.signature(),
            Classes::hkModelerNodeTypeAttribute(class) => class.signature(),
            Classes::hkMonitorStreamColorTable(class) => class.signature(),
            Classes::hkMonitorStreamColorTableColorPair(class) => class.signature(),
            Classes::hkMonitorStreamFrameInfo(class) => class.signature(),
            Classes::hkMonitorStreamStringMap(class) => class.signature(),
            Classes::hkMonitorStreamStringMapStringMap(class) => class.signature(),
            Classes::hkMoppBvTreeShapeBase(class) => class.signature(),
            Classes::hkMotionState(class) => class.signature(),
            Classes::hkMultipleVertexBuffer(class) => class.signature(),
            Classes::hkMultipleVertexBufferElementInfo(class) => class.signature(),
            Classes::hkMultipleVertexBufferLockedElement(class) => class.signature(),
            Classes::hkMultipleVertexBufferVertexBufferInfo(class) => class.signature(),
            Classes::hkMultiThreadCheck(class) => class.signature(),
            Classes::hkp2dAngConstraintAtom(class) => class.signature(),
            Classes::hkpAabbPhantom(class) => class.signature(),
            Classes::hkPackedVector3(class) => class.signature(),
            Classes::hkPackfileHeader(class) => class.signature(),
            Classes::hkPackfileSectionHeader(class) => class.signature(),
            Classes::hkpAction(class) => class.signature(),
            Classes::hkpAgent1nSector(class) => class.signature(),
            Classes::hkpAngConstraintAtom(class) => class.signature(),
            Classes::hkpAngFrictionConstraintAtom(class) => class.signature(),
            Classes::hkpAngLimitConstraintAtom(class) => class.signature(),
            Classes::hkpAngMotorConstraintAtom(class) => class.signature(),
            Classes::hkpAngularDashpotAction(class) => class.signature(),
            Classes::hkpArrayAction(class) => class.signature(),
            Classes::hkpBallAndSocketConstraintData(class) => class.signature(),
            Classes::hkpBallAndSocketConstraintDataAtoms(class) => class.signature(),
            Classes::hkpBallGun(class) => class.signature(),
            Classes::hkpBallSocketChainData(class) => class.signature(),
            Classes::hkpBallSocketChainDataConstraintInfo(class) => class.signature(),
            Classes::hkpBallSocketConstraintAtom(class) => class.signature(),
            Classes::hkpBinaryAction(class) => class.signature(),
            Classes::hkpBoxMotion(class) => class.signature(),
            Classes::hkpBoxShape(class) => class.signature(),
            Classes::hkpBreakableBody(class) => class.signature(),
            Classes::hkpBreakableConstraintData(class) => class.signature(),
            Classes::hkpBridgeAtoms(class) => class.signature(),
            Classes::hkpBridgeConstraintAtom(class) => class.signature(),
            Classes::hkpBroadPhaseHandle(class) => class.signature(),
            Classes::hkpBvShape(class) => class.signature(),
            Classes::hkpBvTreeShape(class) => class.signature(),
            Classes::hkpCachingShapePhantom(class) => class.signature(),
            Classes::hkpCallbackConstraintMotor(class) => class.signature(),
            Classes::hkpCapsuleShape(class) => class.signature(),
            Classes::hkpCdBody(class) => class.signature(),
            Classes::hkpCenterOfMassChangerModifierConstraintAtom(class) => {
                class.signature()
            }
            Classes::hkpCharacterControllerCinfo(class) => class.signature(),
            Classes::hkpCharacterMotion(class) => class.signature(),
            Classes::hkpCharacterProxyCinfo(class) => class.signature(),
            Classes::hkpCharacterRigidBodyCinfo(class) => class.signature(),
            Classes::hkpCogWheelConstraintAtom(class) => class.signature(),
            Classes::hkpCogWheelConstraintData(class) => class.signature(),
            Classes::hkpCogWheelConstraintDataAtoms(class) => class.signature(),
            Classes::hkpCollidable(class) => class.signature(),
            Classes::hkpCollidableBoundingVolumeData(class) => class.signature(),
            Classes::hkpCollidableCollidableFilter(class) => class.signature(),
            Classes::hkpCollisionFilter(class) => class.signature(),
            Classes::hkpCollisionFilterList(class) => class.signature(),
            Classes::hkpCompressedMeshShape(class) => class.signature(),
            Classes::hkpCompressedMeshShapeBigTriangle(class) => class.signature(),
            Classes::hkpCompressedMeshShapeChunk(class) => class.signature(),
            Classes::hkpCompressedMeshShapeConvexPiece(class) => class.signature(),
            Classes::hkpCompressedSampledHeightFieldShape(class) => class.signature(),
            Classes::hkpConeLimitConstraintAtom(class) => class.signature(),
            Classes::hkpConstrainedSystemFilter(class) => class.signature(),
            Classes::hkpConstraintAtom(class) => class.signature(),
            Classes::hkpConstraintChainData(class) => class.signature(),
            Classes::hkpConstraintChainInstance(class) => class.signature(),
            Classes::hkpConstraintChainInstanceAction(class) => class.signature(),
            Classes::hkpConstraintCollisionFilter(class) => class.signature(),
            Classes::hkpConstraintData(class) => class.signature(),
            Classes::hkpConstraintInstance(class) => class.signature(),
            Classes::hkpConstraintInstanceSmallArraySerializeOverrideType(class) => {
                class.signature()
            }
            Classes::hkpConstraintMotor(class) => class.signature(),
            Classes::hkpConvexListFilter(class) => class.signature(),
            Classes::hkpConvexListShape(class) => class.signature(),
            Classes::hkpConvexPieceMeshShape(class) => class.signature(),
            Classes::hkpConvexPieceStreamData(class) => class.signature(),
            Classes::hkpConvexShape(class) => class.signature(),
            Classes::hkpConvexTransformShape(class) => class.signature(),
            Classes::hkpConvexTransformShapeBase(class) => class.signature(),
            Classes::hkpConvexTranslateShape(class) => class.signature(),
            Classes::hkpConvexVerticesConnectivity(class) => class.signature(),
            Classes::hkpConvexVerticesShape(class) => class.signature(),
            Classes::hkpConvexVerticesShapeFourVectors(class) => class.signature(),
            Classes::hkpCylinderShape(class) => class.signature(),
            Classes::hkpDashpotAction(class) => class.signature(),
            Classes::hkpDefaultConvexListFilter(class) => class.signature(),
            Classes::hkpDefaultWorldMemoryWatchDog(class) => class.signature(),
            Classes::hkpDisableEntityCollisionFilter(class) => class.signature(),
            Classes::hkpDisplayBindingData(class) => class.signature(),
            Classes::hkpDisplayBindingDataPhysicsSystem(class) => class.signature(),
            Classes::hkpDisplayBindingDataRigidBody(class) => class.signature(),
            Classes::hkpEntity(class) => class.signature(),
            Classes::hkpEntityExtendedListeners(class) => class.signature(),
            Classes::hkpEntitySmallArraySerializeOverrideType(class) => class.signature(),
            Classes::hkpEntitySpuCollisionCallback(class) => class.signature(),
            Classes::hkpExtendedMeshShape(class) => class.signature(),
            Classes::hkpExtendedMeshShapeShapesSubpart(class) => class.signature(),
            Classes::hkpExtendedMeshShapeSubpart(class) => class.signature(),
            Classes::hkpExtendedMeshShapeTrianglesSubpart(class) => class.signature(),
            Classes::hkpFastMeshShape(class) => class.signature(),
            Classes::hkpFirstPersonGun(class) => class.signature(),
            Classes::hkpFixedRigidMotion(class) => class.signature(),
            Classes::hkpGenericConstraintData(class) => class.signature(),
            Classes::hkpGenericConstraintDataScheme(class) => class.signature(),
            Classes::hkpGenericConstraintDataSchemeConstraintInfo(class) => {
                class.signature()
            }
            Classes::hkpGravityGun(class) => class.signature(),
            Classes::hkpGroupCollisionFilter(class) => class.signature(),
            Classes::hkpGroupFilter(class) => class.signature(),
            Classes::hkpHeightFieldShape(class) => class.signature(),
            Classes::hkpHingeConstraintData(class) => class.signature(),
            Classes::hkpHingeConstraintDataAtoms(class) => class.signature(),
            Classes::hkpHingeLimitsData(class) => class.signature(),
            Classes::hkpHingeLimitsDataAtoms(class) => class.signature(),
            Classes::hkpIgnoreModifierConstraintAtom(class) => class.signature(),
            Classes::hkpKeyframedRigidMotion(class) => class.signature(),
            Classes::hkpLimitedForceConstraintMotor(class) => class.signature(),
            Classes::hkpLimitedHingeConstraintData(class) => class.signature(),
            Classes::hkpLimitedHingeConstraintDataAtoms(class) => class.signature(),
            Classes::hkpLinConstraintAtom(class) => class.signature(),
            Classes::hkpLinearParametricCurve(class) => class.signature(),
            Classes::hkpLinFrictionConstraintAtom(class) => class.signature(),
            Classes::hkpLinkedCollidable(class) => class.signature(),
            Classes::hkpLinLimitConstraintAtom(class) => class.signature(),
            Classes::hkpLinMotorConstraintAtom(class) => class.signature(),
            Classes::hkpLinSoftConstraintAtom(class) => class.signature(),
            Classes::hkpListShape(class) => class.signature(),
            Classes::hkpListShapeChildInfo(class) => class.signature(),
            Classes::hkpMalleableConstraintData(class) => class.signature(),
            Classes::hkpMassChangerModifierConstraintAtom(class) => class.signature(),
            Classes::hkpMassProperties(class) => class.signature(),
            Classes::hkpMaterial(class) => class.signature(),
            Classes::hkpMaxSizeMotion(class) => class.signature(),
            Classes::hkpMeshMaterial(class) => class.signature(),
            Classes::hkpMeshShape(class) => class.signature(),
            Classes::hkpMeshShapeSubpart(class) => class.signature(),
            Classes::hkpModifierConstraintAtom(class) => class.signature(),
            Classes::hkpMoppBvTreeShape(class) => class.signature(),
            Classes::hkpMoppCode(class) => class.signature(),
            Classes::hkpMoppCodeCodeInfo(class) => class.signature(),
            Classes::hkpMoppCodeReindexedTerminal(class) => class.signature(),
            Classes::hkpMotion(class) => class.signature(),
            Classes::hkpMotorAction(class) => class.signature(),
            Classes::hkpMountedBallGun(class) => class.signature(),
            Classes::hkpMouseSpringAction(class) => class.signature(),
            Classes::hkpMovingSurfaceModifierConstraintAtom(class) => class.signature(),
            Classes::hkpMultiRayShape(class) => class.signature(),
            Classes::hkpMultiRayShapeRay(class) => class.signature(),
            Classes::hkpMultiSphereShape(class) => class.signature(),
            Classes::hkpMultithreadedVehicleManager(class) => class.signature(),
            Classes::hkpNamedMeshMaterial(class) => class.signature(),
            Classes::hkpNullCollisionFilter(class) => class.signature(),
            Classes::hkPostFinishAttribute(class) => class.signature(),
            Classes::hkpOverwritePivotConstraintAtom(class) => class.signature(),
            Classes::hkpPairCollisionFilter(class) => class.signature(),
            Classes::hkpPairCollisionFilterMapPairFilterKeyOverrideType(class) => {
                class.signature()
            }
            Classes::hkpParametricCurve(class) => class.signature(),
            Classes::hkpPhantom(class) => class.signature(),
            Classes::hkpPhantomCallbackShape(class) => class.signature(),
            Classes::hkpPhysicsData(class) => class.signature(),
            Classes::hkpPhysicsSystem(class) => class.signature(),
            Classes::hkpPhysicsSystemWithContacts(class) => class.signature(),
            Classes::hkpPlaneShape(class) => class.signature(),
            Classes::hkpPointToPathConstraintData(class) => class.signature(),
            Classes::hkpPointToPlaneConstraintData(class) => class.signature(),
            Classes::hkpPointToPlaneConstraintDataAtoms(class) => class.signature(),
            Classes::hkpPositionConstraintMotor(class) => class.signature(),
            Classes::hkpPoweredChainData(class) => class.signature(),
            Classes::hkpPoweredChainDataConstraintInfo(class) => class.signature(),
            Classes::hkpPoweredChainMapper(class) => class.signature(),
            Classes::hkpPoweredChainMapperLinkInfo(class) => class.signature(),
            Classes::hkpPoweredChainMapperTarget(class) => class.signature(),
            Classes::hkpPrismaticConstraintData(class) => class.signature(),
            Classes::hkpPrismaticConstraintDataAtoms(class) => class.signature(),
            Classes::hkpProjectileGun(class) => class.signature(),
            Classes::hkpProperty(class) => class.signature(),
            Classes::hkpPropertyValue(class) => class.signature(),
            Classes::hkpPulleyConstraintAtom(class) => class.signature(),
            Classes::hkpPulleyConstraintData(class) => class.signature(),
            Classes::hkpPulleyConstraintDataAtoms(class) => class.signature(),
            Classes::hkpRackAndPinionConstraintAtom(class) => class.signature(),
            Classes::hkpRackAndPinionConstraintData(class) => class.signature(),
            Classes::hkpRackAndPinionConstraintDataAtoms(class) => class.signature(),
            Classes::hkpRagdollConstraintData(class) => class.signature(),
            Classes::hkpRagdollConstraintDataAtoms(class) => class.signature(),
            Classes::hkpRagdollLimitsData(class) => class.signature(),
            Classes::hkpRagdollLimitsDataAtoms(class) => class.signature(),
            Classes::hkpRagdollMotorConstraintAtom(class) => class.signature(),
            Classes::hkpRayCollidableFilter(class) => class.signature(),
            Classes::hkpRayShapeCollectionFilter(class) => class.signature(),
            Classes::hkpRejectChassisListener(class) => class.signature(),
            Classes::hkpRemoveTerminalsMoppModifier(class) => class.signature(),
            Classes::hkpReorientAction(class) => class.signature(),
            Classes::hkpRigidBody(class) => class.signature(),
            Classes::hkpRotationalConstraintData(class) => class.signature(),
            Classes::hkpRotationalConstraintDataAtoms(class) => class.signature(),
            Classes::hkpSampledHeightFieldShape(class) => class.signature(),
            Classes::hkpSerializedAgentNnEntry(class) => class.signature(),
            Classes::hkpSerializedDisplayMarker(class) => class.signature(),
            Classes::hkpSerializedDisplayMarkerList(class) => class.signature(),
            Classes::hkpSerializedDisplayRbTransforms(class) => class.signature(),
            Classes::hkpSerializedDisplayRbTransformsDisplayTransformPair(class) => {
                class.signature()
            }
            Classes::hkpSerializedSubTrack1nInfo(class) => class.signature(),
            Classes::hkpSerializedTrack1nInfo(class) => class.signature(),
            Classes::hkpSetLocalRotationsConstraintAtom(class) => class.signature(),
            Classes::hkpSetLocalTransformsConstraintAtom(class) => class.signature(),
            Classes::hkpSetLocalTranslationsConstraintAtom(class) => class.signature(),
            Classes::hkpSetupStabilizationAtom(class) => class.signature(),
            Classes::hkpShape(class) => class.signature(),
            Classes::hkpShapeCollection(class) => class.signature(),
            Classes::hkpShapeCollectionFilter(class) => class.signature(),
            Classes::hkpShapeContainer(class) => class.signature(),
            Classes::hkpShapeInfo(class) => class.signature(),
            Classes::hkpShapeModifier(class) => class.signature(),
            Classes::hkpShapePhantom(class) => class.signature(),
            Classes::hkpSimpleContactConstraintAtom(class) => class.signature(),
            Classes::hkpSimpleContactConstraintDataInfo(class) => class.signature(),
            Classes::hkpSimpleMeshShape(class) => class.signature(),
            Classes::hkpSimpleMeshShapeTriangle(class) => class.signature(),
            Classes::hkpSimpleShapePhantom(class) => class.signature(),
            Classes::hkpSimpleShapePhantomCollisionDetail(class) => class.signature(),
            Classes::hkpSimulation(class) => class.signature(),
            Classes::hkpSingleShapeContainer(class) => class.signature(),
            Classes::hkpSoftContactModifierConstraintAtom(class) => class.signature(),
            Classes::hkpSphereMotion(class) => class.signature(),
            Classes::hkpSphereRepShape(class) => class.signature(),
            Classes::hkpSphereShape(class) => class.signature(),
            Classes::hkpSpringAction(class) => class.signature(),
            Classes::hkpSpringDamperConstraintMotor(class) => class.signature(),
            Classes::hkpStiffSpringChainData(class) => class.signature(),
            Classes::hkpStiffSpringChainDataConstraintInfo(class) => class.signature(),
            Classes::hkpStiffSpringConstraintAtom(class) => class.signature(),
            Classes::hkpStiffSpringConstraintData(class) => class.signature(),
            Classes::hkpStiffSpringConstraintDataAtoms(class) => class.signature(),
            Classes::hkpStorageExtendedMeshShape(class) => class.signature(),
            Classes::hkpStorageExtendedMeshShapeMaterial(class) => class.signature(),
            Classes::hkpStorageExtendedMeshShapeMeshSubpartStorage(class) => {
                class.signature()
            }
            Classes::hkpStorageExtendedMeshShapeShapeSubpartStorage(class) => {
                class.signature()
            }
            Classes::hkpStorageMeshShape(class) => class.signature(),
            Classes::hkpStorageMeshShapeSubpartStorage(class) => class.signature(),
            Classes::hkpStorageSampledHeightFieldShape(class) => class.signature(),
            Classes::hkpThinBoxMotion(class) => class.signature(),
            Classes::hkpTransformShape(class) => class.signature(),
            Classes::hkpTriangleShape(class) => class.signature(),
            Classes::hkpTriggerVolume(class) => class.signature(),
            Classes::hkpTriggerVolumeEventInfo(class) => class.signature(),
            Classes::hkpTriSampledHeightFieldBvTreeShape(class) => class.signature(),
            Classes::hkpTriSampledHeightFieldCollection(class) => class.signature(),
            Classes::hkpTwistLimitConstraintAtom(class) => class.signature(),
            Classes::hkpTypedBroadPhaseHandle(class) => class.signature(),
            Classes::hkpTyremarkPoint(class) => class.signature(),
            Classes::hkpTyremarksInfo(class) => class.signature(),
            Classes::hkpTyremarksWheel(class) => class.signature(),
            Classes::hkpUnaryAction(class) => class.signature(),
            Classes::hkpVehicleAerodynamics(class) => class.signature(),
            Classes::hkpVehicleBrake(class) => class.signature(),
            Classes::hkpVehicleCastBatchingManager(class) => class.signature(),
            Classes::hkpVehicleData(class) => class.signature(),
            Classes::hkpVehicleDataWheelComponentParams(class) => class.signature(),
            Classes::hkpVehicleDefaultAerodynamics(class) => class.signature(),
            Classes::hkpVehicleDefaultAnalogDriverInput(class) => class.signature(),
            Classes::hkpVehicleDefaultBrake(class) => class.signature(),
            Classes::hkpVehicleDefaultBrakeWheelBrakingProperties(class) => {
                class.signature()
            }
            Classes::hkpVehicleDefaultEngine(class) => class.signature(),
            Classes::hkpVehicleDefaultSteering(class) => class.signature(),
            Classes::hkpVehicleDefaultSuspension(class) => class.signature(),
            Classes::hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters(
                class,
            ) => class.signature(),
            Classes::hkpVehicleDefaultTransmission(class) => class.signature(),
            Classes::hkpVehicleDefaultVelocityDamper(class) => class.signature(),
            Classes::hkpVehicleDriverInput(class) => class.signature(),
            Classes::hkpVehicleDriverInputAnalogStatus(class) => class.signature(),
            Classes::hkpVehicleDriverInputStatus(class) => class.signature(),
            Classes::hkpVehicleEngine(class) => class.signature(),
            Classes::hkpVehicleFrictionDescription(class) => class.signature(),
            Classes::hkpVehicleFrictionDescriptionAxisDescription(class) => {
                class.signature()
            }
            Classes::hkpVehicleFrictionStatus(class) => class.signature(),
            Classes::hkpVehicleFrictionStatusAxisStatus(class) => class.signature(),
            Classes::hkpVehicleInstance(class) => class.signature(),
            Classes::hkpVehicleInstanceWheelInfo(class) => class.signature(),
            Classes::hkpVehicleLinearCastBatchingManager(class) => class.signature(),
            Classes::hkpVehicleLinearCastWheelCollide(class) => class.signature(),
            Classes::hkpVehicleLinearCastWheelCollideWheelState(class) => {
                class.signature()
            }
            Classes::hkpVehicleManager(class) => class.signature(),
            Classes::hkpVehicleRayCastBatchingManager(class) => class.signature(),
            Classes::hkpVehicleRayCastWheelCollide(class) => class.signature(),
            Classes::hkpVehicleSteering(class) => class.signature(),
            Classes::hkpVehicleSuspension(class) => class.signature(),
            Classes::hkpVehicleSuspensionSuspensionWheelParameters(class) => {
                class.signature()
            }
            Classes::hkpVehicleTransmission(class) => class.signature(),
            Classes::hkpVehicleVelocityDamper(class) => class.signature(),
            Classes::hkpVehicleWheelCollide(class) => class.signature(),
            Classes::hkpVelocityConstraintMotor(class) => class.signature(),
            Classes::hkpViscousSurfaceModifierConstraintAtom(class) => class.signature(),
            Classes::hkpWeldingUtility(class) => class.signature(),
            Classes::hkpWheelConstraintData(class) => class.signature(),
            Classes::hkpWheelConstraintDataAtoms(class) => class.signature(),
            Classes::hkpWorld(class) => class.signature(),
            Classes::hkpWorldCinfo(class) => class.signature(),
            Classes::hkpWorldObject(class) => class.signature(),
            Classes::hkQTransform(class) => class.signature(),
            Classes::hkRangeInt32Attribute(class) => class.signature(),
            Classes::hkRangeRealAttribute(class) => class.signature(),
            Classes::hkReferencedObject(class) => class.signature(),
            Classes::hkReflectedFileAttribute(class) => class.signature(),
            Classes::hkResourceBase(class) => class.signature(),
            Classes::hkResourceContainer(class) => class.signature(),
            Classes::hkResourceHandle(class) => class.signature(),
            Classes::hkRootLevelContainer(class) => class.signature(),
            Classes::hkRootLevelContainerNamedVariant(class) => class.signature(),
            Classes::hkSemanticsAttribute(class) => class.signature(),
            Classes::hkSimpleLocalFrame(class) => class.signature(),
            Classes::hkSphere(class) => class.signature(),
            Classes::hkSweptTransform(class) => class.signature(),
            Classes::hkTraceStreamTitle(class) => class.signature(),
            Classes::hkTrackerSerializableScanSnapshot(class) => class.signature(),
            Classes::hkTrackerSerializableScanSnapshotAllocation(class) => {
                class.signature()
            }
            Classes::hkTrackerSerializableScanSnapshotBlock(class) => class.signature(),
            Classes::hkUiAttribute(class) => class.signature(),
            Classes::hkVertexFormat(class) => class.signature(),
            Classes::hkVertexFormatElement(class) => class.signature(),
            Classes::hkWorldMemoryAvailableWatchDog(class) => class.signature(),
            Classes::hkxAnimatedFloat(class) => class.signature(),
            Classes::hkxAnimatedMatrix(class) => class.signature(),
            Classes::hkxAnimatedQuaternion(class) => class.signature(),
            Classes::hkxAnimatedVector(class) => class.signature(),
            Classes::hkxAttribute(class) => class.signature(),
            Classes::hkxAttributeGroup(class) => class.signature(),
            Classes::hkxAttributeHolder(class) => class.signature(),
            Classes::hkxCamera(class) => class.signature(),
            Classes::hkxEdgeSelectionChannel(class) => class.signature(),
            Classes::hkxEnum(class) => class.signature(),
            Classes::hkxEnumItem(class) => class.signature(),
            Classes::hkxEnvironment(class) => class.signature(),
            Classes::hkxEnvironmentVariable(class) => class.signature(),
            Classes::hkxIndexBuffer(class) => class.signature(),
            Classes::hkxLight(class) => class.signature(),
            Classes::hkxMaterial(class) => class.signature(),
            Classes::hkxMaterialEffect(class) => class.signature(),
            Classes::hkxMaterialProperty(class) => class.signature(),
            Classes::hkxMaterialShader(class) => class.signature(),
            Classes::hkxMaterialShaderSet(class) => class.signature(),
            Classes::hkxMaterialTextureStage(class) => class.signature(),
            Classes::hkxMesh(class) => class.signature(),
            Classes::hkxMeshSection(class) => class.signature(),
            Classes::hkxMeshUserChannelInfo(class) => class.signature(),
            Classes::hkxNode(class) => class.signature(),
            Classes::hkxNodeAnnotationData(class) => class.signature(),
            Classes::hkxNodeSelectionSet(class) => class.signature(),
            Classes::hkxScene(class) => class.signature(),
            Classes::hkxSkinBinding(class) => class.signature(),
            Classes::hkxSparselyAnimatedBool(class) => class.signature(),
            Classes::hkxSparselyAnimatedEnum(class) => class.signature(),
            Classes::hkxSparselyAnimatedInt(class) => class.signature(),
            Classes::hkxSparselyAnimatedString(class) => class.signature(),
            Classes::hkxTextureFile(class) => class.signature(),
            Classes::hkxTextureInplace(class) => class.signature(),
            Classes::hkxTriangleSelectionChannel(class) => class.signature(),
            Classes::hkxVertexBuffer(class) => class.signature(),
            Classes::hkxVertexBufferVertexData(class) => class.signature(),
            Classes::hkxVertexDescription(class) => class.signature(),
            Classes::hkxVertexDescriptionElementDecl(class) => class.signature(),
            Classes::hkxVertexFloatDataChannel(class) => class.signature(),
            Classes::hkxVertexIntDataChannel(class) => class.signature(),
            Classes::hkxVertexSelectionChannel(class) => class.signature(),
            Classes::hkxVertexVectorDataChannel(class) => class.signature(),
        }
    }
    fn deps_indexes(&self) -> Vec<&havok_types::Pointer<'_>> {
        match &self {
            Classes::BGSGamebryoSequenceGenerator(class) => class.deps_indexes(),
            Classes::BSBoneSwitchGenerator(class) => class.deps_indexes(),
            Classes::BSBoneSwitchGeneratorBoneData(class) => class.deps_indexes(),
            Classes::BSComputeAddBoneAnimModifier(class) => class.deps_indexes(),
            Classes::BSCyclicBlendTransitionGenerator(class) => class.deps_indexes(),
            Classes::BSDecomposeVectorModifier(class) => class.deps_indexes(),
            Classes::BSDirectAtModifier(class) => class.deps_indexes(),
            Classes::BSDistTriggerModifier(class) => class.deps_indexes(),
            Classes::BSEventEveryNEventsModifier(class) => class.deps_indexes(),
            Classes::BSEventOnDeactivateModifier(class) => class.deps_indexes(),
            Classes::BSEventOnFalseToTrueModifier(class) => class.deps_indexes(),
            Classes::BSGetTimeStepModifier(class) => class.deps_indexes(),
            Classes::BSInterpValueModifier(class) => class.deps_indexes(),
            Classes::BSIsActiveModifier(class) => class.deps_indexes(),
            Classes::BSIStateManagerModifier(class) => class.deps_indexes(),
            Classes::BSIStateManagerModifierBSiStateData(class) => class.deps_indexes(),
            Classes::BSIStateManagerModifierBSIStateManagerStateListener(class) => {
                class.deps_indexes()
            }
            Classes::BSiStateTaggingGenerator(class) => class.deps_indexes(),
            Classes::BSLimbIKModifier(class) => class.deps_indexes(),
            Classes::BSLookAtModifier(class) => class.deps_indexes(),
            Classes::BSLookAtModifierBoneData(class) => class.deps_indexes(),
            Classes::BSModifyOnceModifier(class) => class.deps_indexes(),
            Classes::BSOffsetAnimationGenerator(class) => class.deps_indexes(),
            Classes::BSPassByTargetTriggerModifier(class) => class.deps_indexes(),
            Classes::BSRagdollContactListenerModifier(class) => class.deps_indexes(),
            Classes::BSSpeedSamplerModifier(class) => class.deps_indexes(),
            Classes::BSSynchronizedClipGenerator(class) => class.deps_indexes(),
            Classes::BSTimerModifier(class) => class.deps_indexes(),
            Classes::BSTweenerModifier(class) => class.deps_indexes(),
            Classes::hkAabb(class) => class.deps_indexes(),
            Classes::hkAabbHalf(class) => class.deps_indexes(),
            Classes::hkAabbUint32(class) => class.deps_indexes(),
            Classes::hkaAnimatedReferenceFrame(class) => class.deps_indexes(),
            Classes::hkaAnimation(class) => class.deps_indexes(),
            Classes::hkaAnimationBinding(class) => class.deps_indexes(),
            Classes::hkaAnimationContainer(class) => class.deps_indexes(),
            Classes::hkaAnimationPreviewColorContainer(class) => class.deps_indexes(),
            Classes::hkaAnnotationTrack(class) => class.deps_indexes(),
            Classes::hkaAnnotationTrackAnnotation(class) => class.deps_indexes(),
            Classes::hkaBone(class) => class.deps_indexes(),
            Classes::hkaBoneAttachment(class) => class.deps_indexes(),
            Classes::hkaDefaultAnimatedReferenceFrame(class) => class.deps_indexes(),
            Classes::hkaDeltaCompressedAnimation(class) => class.deps_indexes(),
            Classes::hkaDeltaCompressedAnimationQuantizationFormat(class) => {
                class.deps_indexes()
            }
            Classes::hkaFootstepAnalysisInfo(class) => class.deps_indexes(),
            Classes::hkaFootstepAnalysisInfoContainer(class) => class.deps_indexes(),
            Classes::hkaInterleavedUncompressedAnimation(class) => class.deps_indexes(),
            Classes::hkaKeyFrameHierarchyUtility(class) => class.deps_indexes(),
            Classes::hkaKeyFrameHierarchyUtilityControlData(class) => {
                class.deps_indexes()
            }
            Classes::hkAlignSceneToNodeOptions(class) => class.deps_indexes(),
            Classes::hkaMeshBinding(class) => class.deps_indexes(),
            Classes::hkaMeshBindingMapping(class) => class.deps_indexes(),
            Classes::hkaQuantizedAnimation(class) => class.deps_indexes(),
            Classes::hkaQuantizedAnimationTrackCompressionParams(class) => {
                class.deps_indexes()
            }
            Classes::hkaRagdollInstance(class) => class.deps_indexes(),
            Classes::hkArrayTypeAttribute(class) => class.deps_indexes(),
            Classes::hkaSkeleton(class) => class.deps_indexes(),
            Classes::hkaSkeletonLocalFrameOnBone(class) => class.deps_indexes(),
            Classes::hkaSkeletonMapper(class) => class.deps_indexes(),
            Classes::hkaSkeletonMapperData(class) => class.deps_indexes(),
            Classes::hkaSkeletonMapperDataChainMapping(class) => class.deps_indexes(),
            Classes::hkaSkeletonMapperDataSimpleMapping(class) => class.deps_indexes(),
            Classes::hkaSplineCompressedAnimation(class) => class.deps_indexes(),
            Classes::hkaSplineCompressedAnimationAnimationCompressionParams(class) => {
                class.deps_indexes()
            }
            Classes::hkaSplineCompressedAnimationTrackCompressionParams(class) => {
                class.deps_indexes()
            }
            Classes::hkaWaveletCompressedAnimation(class) => class.deps_indexes(),
            Classes::hkaWaveletCompressedAnimationCompressionParams(class) => {
                class.deps_indexes()
            }
            Classes::hkaWaveletCompressedAnimationQuantizationFormat(class) => {
                class.deps_indexes()
            }
            Classes::hkBaseObject(class) => class.deps_indexes(),
            Classes::hkbAttachmentModifier(class) => class.deps_indexes(),
            Classes::hkbAttachmentSetup(class) => class.deps_indexes(),
            Classes::hkbAttributeModifier(class) => class.deps_indexes(),
            Classes::hkbAttributeModifierAssignment(class) => class.deps_indexes(),
            Classes::hkbAuxiliaryNodeInfo(class) => class.deps_indexes(),
            Classes::hkbBehaviorEventsInfo(class) => class.deps_indexes(),
            Classes::hkbBehaviorGraph(class) => class.deps_indexes(),
            Classes::hkbBehaviorGraphData(class) => class.deps_indexes(),
            Classes::hkbBehaviorGraphInternalState(class) => class.deps_indexes(),
            Classes::hkbBehaviorGraphInternalStateInfo(class) => class.deps_indexes(),
            Classes::hkbBehaviorGraphStringData(class) => class.deps_indexes(),
            Classes::hkbBehaviorInfo(class) => class.deps_indexes(),
            Classes::hkbBehaviorInfoIdToNamePair(class) => class.deps_indexes(),
            Classes::hkbBehaviorReferenceGenerator(class) => class.deps_indexes(),
            Classes::hkbBindable(class) => class.deps_indexes(),
            Classes::hkbBlendCurveUtils(class) => class.deps_indexes(),
            Classes::hkbBlenderGenerator(class) => class.deps_indexes(),
            Classes::hkbBlenderGeneratorChild(class) => class.deps_indexes(),
            Classes::hkbBlenderGeneratorChildInternalState(class) => class.deps_indexes(),
            Classes::hkbBlenderGeneratorInternalState(class) => class.deps_indexes(),
            Classes::hkbBlendingTransitionEffect(class) => class.deps_indexes(),
            Classes::hkbBlendingTransitionEffectInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbBoneIndexArray(class) => class.deps_indexes(),
            Classes::hkbBoneWeightArray(class) => class.deps_indexes(),
            Classes::hkbBoolVariableSequencedData(class) => class.deps_indexes(),
            Classes::hkbBoolVariableSequencedDataSample(class) => class.deps_indexes(),
            Classes::hkbCameraShakeEventPayload(class) => class.deps_indexes(),
            Classes::hkbCharacter(class) => class.deps_indexes(),
            Classes::hkbCharacterAddedInfo(class) => class.deps_indexes(),
            Classes::hkbCharacterControlCommand(class) => class.deps_indexes(),
            Classes::hkbCharacterControllerControlData(class) => class.deps_indexes(),
            Classes::hkbCharacterControllerModifier(class) => class.deps_indexes(),
            Classes::hkbCharacterControllerModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbCharacterData(class) => class.deps_indexes(),
            Classes::hkbCharacterDataCharacterControllerInfo(class) => {
                class.deps_indexes()
            }
            Classes::hkbCharacterInfo(class) => class.deps_indexes(),
            Classes::hkbCharacterSetup(class) => class.deps_indexes(),
            Classes::hkbCharacterSkinInfo(class) => class.deps_indexes(),
            Classes::hkbCharacterSteppedInfo(class) => class.deps_indexes(),
            Classes::hkbCharacterStringData(class) => class.deps_indexes(),
            Classes::hkbClientCharacterState(class) => class.deps_indexes(),
            Classes::hkbClipGenerator(class) => class.deps_indexes(),
            Classes::hkbClipGeneratorEcho(class) => class.deps_indexes(),
            Classes::hkbClipGeneratorInternalState(class) => class.deps_indexes(),
            Classes::hkbClipTrigger(class) => class.deps_indexes(),
            Classes::hkbClipTriggerArray(class) => class.deps_indexes(),
            Classes::hkbCombineTransformsModifier(class) => class.deps_indexes(),
            Classes::hkbCombineTransformsModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbCompiledExpressionSet(class) => class.deps_indexes(),
            Classes::hkbCompiledExpressionSetToken(class) => class.deps_indexes(),
            Classes::hkbComputeDirectionModifier(class) => class.deps_indexes(),
            Classes::hkbComputeDirectionModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbComputeRotationFromAxisAngleModifier(class) => {
                class.deps_indexes()
            }
            Classes::hkbComputeRotationFromAxisAngleModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbComputeRotationToTargetModifier(class) => class.deps_indexes(),
            Classes::hkbComputeRotationToTargetModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbCondition(class) => class.deps_indexes(),
            Classes::hkbContext(class) => class.deps_indexes(),
            Classes::hkbDampingModifier(class) => class.deps_indexes(),
            Classes::hkbDampingModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbDefaultMessageLog(class) => class.deps_indexes(),
            Classes::hkbDelayedModifier(class) => class.deps_indexes(),
            Classes::hkbDelayedModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbDetectCloseToGroundModifier(class) => class.deps_indexes(),
            Classes::hkbDetectCloseToGroundModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbEvaluateExpressionModifier(class) => class.deps_indexes(),
            Classes::hkbEvaluateExpressionModifierInternalExpressionData(class) => {
                class.deps_indexes()
            }
            Classes::hkbEvaluateExpressionModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbEvaluateHandleModifier(class) => class.deps_indexes(),
            Classes::hkbEvent(class) => class.deps_indexes(),
            Classes::hkbEventBase(class) => class.deps_indexes(),
            Classes::hkbEventDrivenModifier(class) => class.deps_indexes(),
            Classes::hkbEventDrivenModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbEventInfo(class) => class.deps_indexes(),
            Classes::hkbEventPayload(class) => class.deps_indexes(),
            Classes::hkbEventPayloadList(class) => class.deps_indexes(),
            Classes::hkbEventProperty(class) => class.deps_indexes(),
            Classes::hkbEventRaisedInfo(class) => class.deps_indexes(),
            Classes::hkbEventRangeData(class) => class.deps_indexes(),
            Classes::hkbEventRangeDataArray(class) => class.deps_indexes(),
            Classes::hkbEventSequencedData(class) => class.deps_indexes(),
            Classes::hkbEventSequencedDataSequencedEvent(class) => class.deps_indexes(),
            Classes::hkbEventsFromRangeModifier(class) => class.deps_indexes(),
            Classes::hkbEventsFromRangeModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbExpressionCondition(class) => class.deps_indexes(),
            Classes::hkbExpressionData(class) => class.deps_indexes(),
            Classes::hkbExpressionDataArray(class) => class.deps_indexes(),
            Classes::hkbExtractRagdollPoseModifier(class) => class.deps_indexes(),
            Classes::hkbFootIkControlData(class) => class.deps_indexes(),
            Classes::hkbFootIkControlsModifier(class) => class.deps_indexes(),
            Classes::hkbFootIkControlsModifierLeg(class) => class.deps_indexes(),
            Classes::hkbFootIkDriverInfo(class) => class.deps_indexes(),
            Classes::hkbFootIkDriverInfoLeg(class) => class.deps_indexes(),
            Classes::hkbFootIkGains(class) => class.deps_indexes(),
            Classes::hkbFootIkModifier(class) => class.deps_indexes(),
            Classes::hkbFootIkModifierInternalLegData(class) => class.deps_indexes(),
            Classes::hkbFootIkModifierLeg(class) => class.deps_indexes(),
            Classes::hkbGenerator(class) => class.deps_indexes(),
            Classes::hkbGeneratorOutputListener(class) => class.deps_indexes(),
            Classes::hkbGeneratorSyncInfo(class) => class.deps_indexes(),
            Classes::hkbGeneratorSyncInfoSyncPoint(class) => class.deps_indexes(),
            Classes::hkbGeneratorTransitionEffect(class) => class.deps_indexes(),
            Classes::hkbGeneratorTransitionEffectInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbGetHandleOnBoneModifier(class) => class.deps_indexes(),
            Classes::hkbGetUpModifier(class) => class.deps_indexes(),
            Classes::hkbGetUpModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbGetWorldFromModelModifier(class) => class.deps_indexes(),
            Classes::hkbGetWorldFromModelModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbHandIkControlData(class) => class.deps_indexes(),
            Classes::hkbHandIkControlsModifier(class) => class.deps_indexes(),
            Classes::hkbHandIkControlsModifierHand(class) => class.deps_indexes(),
            Classes::hkbHandIkDriverInfo(class) => class.deps_indexes(),
            Classes::hkbHandIkDriverInfoHand(class) => class.deps_indexes(),
            Classes::hkbHandIkModifier(class) => class.deps_indexes(),
            Classes::hkbHandIkModifierHand(class) => class.deps_indexes(),
            Classes::hkbHandle(class) => class.deps_indexes(),
            Classes::hkbIntEventPayload(class) => class.deps_indexes(),
            Classes::hkbIntVariableSequencedData(class) => class.deps_indexes(),
            Classes::hkbIntVariableSequencedDataSample(class) => class.deps_indexes(),
            Classes::hkBitField(class) => class.deps_indexes(),
            Classes::hkbKeyframeBonesModifier(class) => class.deps_indexes(),
            Classes::hkbKeyframeBonesModifierKeyframeInfo(class) => class.deps_indexes(),
            Classes::hkbLinkedSymbolInfo(class) => class.deps_indexes(),
            Classes::hkbLookAtModifier(class) => class.deps_indexes(),
            Classes::hkbLookAtModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbManualSelectorGenerator(class) => class.deps_indexes(),
            Classes::hkbManualSelectorGeneratorInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbMessageLog(class) => class.deps_indexes(),
            Classes::hkbMirroredSkeletonInfo(class) => class.deps_indexes(),
            Classes::hkbMirrorModifier(class) => class.deps_indexes(),
            Classes::hkbModifier(class) => class.deps_indexes(),
            Classes::hkbModifierGenerator(class) => class.deps_indexes(),
            Classes::hkbModifierList(class) => class.deps_indexes(),
            Classes::hkbModifierWrapper(class) => class.deps_indexes(),
            Classes::hkbMoveCharacterModifier(class) => class.deps_indexes(),
            Classes::hkbMoveCharacterModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbNamedEventPayload(class) => class.deps_indexes(),
            Classes::hkbNamedIntEventPayload(class) => class.deps_indexes(),
            Classes::hkbNamedRealEventPayload(class) => class.deps_indexes(),
            Classes::hkbNamedStringEventPayload(class) => class.deps_indexes(),
            Classes::hkbNode(class) => class.deps_indexes(),
            Classes::hkbNodeInternalStateInfo(class) => class.deps_indexes(),
            Classes::hkbParticleSystemEventPayload(class) => class.deps_indexes(),
            Classes::hkbPoseMatchingGenerator(class) => class.deps_indexes(),
            Classes::hkbPoseMatchingGeneratorInternalState(class) => class.deps_indexes(),
            Classes::hkbPoweredRagdollControlData(class) => class.deps_indexes(),
            Classes::hkbPoweredRagdollControlsModifier(class) => class.deps_indexes(),
            Classes::hkbProjectData(class) => class.deps_indexes(),
            Classes::hkbProjectStringData(class) => class.deps_indexes(),
            Classes::hkbProxyModifier(class) => class.deps_indexes(),
            Classes::hkbProxyModifierProxyInfo(class) => class.deps_indexes(),
            Classes::hkbRaiseEventCommand(class) => class.deps_indexes(),
            Classes::hkbRealEventPayload(class) => class.deps_indexes(),
            Classes::hkbRealVariableSequencedData(class) => class.deps_indexes(),
            Classes::hkbRealVariableSequencedDataSample(class) => class.deps_indexes(),
            Classes::hkbReferencePoseGenerator(class) => class.deps_indexes(),
            Classes::hkbRegisteredGenerator(class) => class.deps_indexes(),
            Classes::hkbRigidBodyRagdollControlData(class) => class.deps_indexes(),
            Classes::hkbRigidBodyRagdollControlsModifier(class) => class.deps_indexes(),
            Classes::hkbRoleAttribute(class) => class.deps_indexes(),
            Classes::hkbRotateCharacterModifier(class) => class.deps_indexes(),
            Classes::hkbRotateCharacterModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbSenseHandleModifier(class) => class.deps_indexes(),
            Classes::hkbSenseHandleModifierRange(class) => class.deps_indexes(),
            Classes::hkbSequence(class) => class.deps_indexes(),
            Classes::hkbSequencedData(class) => class.deps_indexes(),
            Classes::hkbSequenceInternalState(class) => class.deps_indexes(),
            Classes::hkbSequenceStringData(class) => class.deps_indexes(),
            Classes::hkbSetBehaviorCommand(class) => class.deps_indexes(),
            Classes::hkbSetLocalTimeOfClipGeneratorCommand(class) => class.deps_indexes(),
            Classes::hkbSetNodePropertyCommand(class) => class.deps_indexes(),
            Classes::hkbSetWordVariableCommand(class) => class.deps_indexes(),
            Classes::hkbSetWorldFromModelModifier(class) => class.deps_indexes(),
            Classes::hkbSimulationControlCommand(class) => class.deps_indexes(),
            Classes::hkbSimulationStateInfo(class) => class.deps_indexes(),
            Classes::hkbStateChooser(class) => class.deps_indexes(),
            Classes::hkbStateListener(class) => class.deps_indexes(),
            Classes::hkbStateMachine(class) => class.deps_indexes(),
            Classes::hkbStateMachineActiveTransitionInfo(class) => class.deps_indexes(),
            Classes::hkbStateMachineDelayedTransitionInfo(class) => class.deps_indexes(),
            Classes::hkbStateMachineEventPropertyArray(class) => class.deps_indexes(),
            Classes::hkbStateMachineInternalState(class) => class.deps_indexes(),
            Classes::hkbStateMachineNestedStateMachineData(class) => class.deps_indexes(),
            Classes::hkbStateMachineProspectiveTransitionInfo(class) => {
                class.deps_indexes()
            }
            Classes::hkbStateMachineStateInfo(class) => class.deps_indexes(),
            Classes::hkbStateMachineTimeInterval(class) => class.deps_indexes(),
            Classes::hkbStateMachineTransitionInfo(class) => class.deps_indexes(),
            Classes::hkbStateMachineTransitionInfoArray(class) => class.deps_indexes(),
            Classes::hkbStateMachineTransitionInfoReference(class) => {
                class.deps_indexes()
            }
            Classes::hkbStringCondition(class) => class.deps_indexes(),
            Classes::hkbStringEventPayload(class) => class.deps_indexes(),
            Classes::hkbTestStateChooser(class) => class.deps_indexes(),
            Classes::hkbTimerModifier(class) => class.deps_indexes(),
            Classes::hkbTimerModifierInternalState(class) => class.deps_indexes(),
            Classes::hkbTransformVectorModifier(class) => class.deps_indexes(),
            Classes::hkbTransformVectorModifierInternalState(class) => {
                class.deps_indexes()
            }
            Classes::hkbTransitionEffect(class) => class.deps_indexes(),
            Classes::hkbTwistModifier(class) => class.deps_indexes(),
            Classes::hkbVariableBindingSet(class) => class.deps_indexes(),
            Classes::hkbVariableBindingSetBinding(class) => class.deps_indexes(),
            Classes::hkbVariableInfo(class) => class.deps_indexes(),
            Classes::hkbVariableValue(class) => class.deps_indexes(),
            Classes::hkbVariableValueSet(class) => class.deps_indexes(),
            Classes::hkbWorldEnums(class) => class.deps_indexes(),
            Classes::hkbWorldFromModelModeData(class) => class.deps_indexes(),
            Classes::hkClass(class) => class.deps_indexes(),
            Classes::hkClassEnum(class) => class.deps_indexes(),
            Classes::hkClassEnumItem(class) => class.deps_indexes(),
            Classes::hkClassMember(class) => class.deps_indexes(),
            Classes::hkColor(class) => class.deps_indexes(),
            Classes::hkContactPoint(class) => class.deps_indexes(),
            Classes::hkContactPointMaterial(class) => class.deps_indexes(),
            Classes::hkCustomAttributes(class) => class.deps_indexes(),
            Classes::hkCustomAttributesAttribute(class) => class.deps_indexes(),
            Classes::hkDataObjectTypeAttribute(class) => class.deps_indexes(),
            Classes::hkDescriptionAttribute(class) => class.deps_indexes(),
            Classes::hkDocumentationAttribute(class) => class.deps_indexes(),
            Classes::hkGeometry(class) => class.deps_indexes(),
            Classes::hkGeometryTriangle(class) => class.deps_indexes(),
            Classes::hkGizmoAttribute(class) => class.deps_indexes(),
            Classes::hkHalf8(class) => class.deps_indexes(),
            Classes::hkIndexedTransformSet(class) => class.deps_indexes(),
            Classes::hkLinkAttribute(class) => class.deps_indexes(),
            Classes::hkLocalFrame(class) => class.deps_indexes(),
            Classes::hkLocalFrameGroup(class) => class.deps_indexes(),
            Classes::hkMemoryMeshBody(class) => class.deps_indexes(),
            Classes::hkMemoryMeshMaterial(class) => class.deps_indexes(),
            Classes::hkMemoryMeshShape(class) => class.deps_indexes(),
            Classes::hkMemoryMeshTexture(class) => class.deps_indexes(),
            Classes::hkMemoryMeshVertexBuffer(class) => class.deps_indexes(),
            Classes::hkMemoryResourceContainer(class) => class.deps_indexes(),
            Classes::hkMemoryResourceHandle(class) => class.deps_indexes(),
            Classes::hkMemoryResourceHandleExternalLink(class) => class.deps_indexes(),
            Classes::hkMemoryTrackerAttribute(class) => class.deps_indexes(),
            Classes::hkMeshBody(class) => class.deps_indexes(),
            Classes::hkMeshBoneIndexMapping(class) => class.deps_indexes(),
            Classes::hkMeshMaterial(class) => class.deps_indexes(),
            Classes::hkMeshSection(class) => class.deps_indexes(),
            Classes::hkMeshSectionCinfo(class) => class.deps_indexes(),
            Classes::hkMeshShape(class) => class.deps_indexes(),
            Classes::hkMeshTexture(class) => class.deps_indexes(),
            Classes::hkMeshVertexBuffer(class) => class.deps_indexes(),
            Classes::hkModelerNodeTypeAttribute(class) => class.deps_indexes(),
            Classes::hkMonitorStreamColorTable(class) => class.deps_indexes(),
            Classes::hkMonitorStreamColorTableColorPair(class) => class.deps_indexes(),
            Classes::hkMonitorStreamFrameInfo(class) => class.deps_indexes(),
            Classes::hkMonitorStreamStringMap(class) => class.deps_indexes(),
            Classes::hkMonitorStreamStringMapStringMap(class) => class.deps_indexes(),
            Classes::hkMoppBvTreeShapeBase(class) => class.deps_indexes(),
            Classes::hkMotionState(class) => class.deps_indexes(),
            Classes::hkMultipleVertexBuffer(class) => class.deps_indexes(),
            Classes::hkMultipleVertexBufferElementInfo(class) => class.deps_indexes(),
            Classes::hkMultipleVertexBufferLockedElement(class) => class.deps_indexes(),
            Classes::hkMultipleVertexBufferVertexBufferInfo(class) => {
                class.deps_indexes()
            }
            Classes::hkMultiThreadCheck(class) => class.deps_indexes(),
            Classes::hkp2dAngConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpAabbPhantom(class) => class.deps_indexes(),
            Classes::hkPackedVector3(class) => class.deps_indexes(),
            Classes::hkPackfileHeader(class) => class.deps_indexes(),
            Classes::hkPackfileSectionHeader(class) => class.deps_indexes(),
            Classes::hkpAction(class) => class.deps_indexes(),
            Classes::hkpAgent1nSector(class) => class.deps_indexes(),
            Classes::hkpAngConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpAngFrictionConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpAngLimitConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpAngMotorConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpAngularDashpotAction(class) => class.deps_indexes(),
            Classes::hkpArrayAction(class) => class.deps_indexes(),
            Classes::hkpBallAndSocketConstraintData(class) => class.deps_indexes(),
            Classes::hkpBallAndSocketConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpBallGun(class) => class.deps_indexes(),
            Classes::hkpBallSocketChainData(class) => class.deps_indexes(),
            Classes::hkpBallSocketChainDataConstraintInfo(class) => class.deps_indexes(),
            Classes::hkpBallSocketConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpBinaryAction(class) => class.deps_indexes(),
            Classes::hkpBoxMotion(class) => class.deps_indexes(),
            Classes::hkpBoxShape(class) => class.deps_indexes(),
            Classes::hkpBreakableBody(class) => class.deps_indexes(),
            Classes::hkpBreakableConstraintData(class) => class.deps_indexes(),
            Classes::hkpBridgeAtoms(class) => class.deps_indexes(),
            Classes::hkpBridgeConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpBroadPhaseHandle(class) => class.deps_indexes(),
            Classes::hkpBvShape(class) => class.deps_indexes(),
            Classes::hkpBvTreeShape(class) => class.deps_indexes(),
            Classes::hkpCachingShapePhantom(class) => class.deps_indexes(),
            Classes::hkpCallbackConstraintMotor(class) => class.deps_indexes(),
            Classes::hkpCapsuleShape(class) => class.deps_indexes(),
            Classes::hkpCdBody(class) => class.deps_indexes(),
            Classes::hkpCenterOfMassChangerModifierConstraintAtom(class) => {
                class.deps_indexes()
            }
            Classes::hkpCharacterControllerCinfo(class) => class.deps_indexes(),
            Classes::hkpCharacterMotion(class) => class.deps_indexes(),
            Classes::hkpCharacterProxyCinfo(class) => class.deps_indexes(),
            Classes::hkpCharacterRigidBodyCinfo(class) => class.deps_indexes(),
            Classes::hkpCogWheelConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpCogWheelConstraintData(class) => class.deps_indexes(),
            Classes::hkpCogWheelConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpCollidable(class) => class.deps_indexes(),
            Classes::hkpCollidableBoundingVolumeData(class) => class.deps_indexes(),
            Classes::hkpCollidableCollidableFilter(class) => class.deps_indexes(),
            Classes::hkpCollisionFilter(class) => class.deps_indexes(),
            Classes::hkpCollisionFilterList(class) => class.deps_indexes(),
            Classes::hkpCompressedMeshShape(class) => class.deps_indexes(),
            Classes::hkpCompressedMeshShapeBigTriangle(class) => class.deps_indexes(),
            Classes::hkpCompressedMeshShapeChunk(class) => class.deps_indexes(),
            Classes::hkpCompressedMeshShapeConvexPiece(class) => class.deps_indexes(),
            Classes::hkpCompressedSampledHeightFieldShape(class) => class.deps_indexes(),
            Classes::hkpConeLimitConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpConstrainedSystemFilter(class) => class.deps_indexes(),
            Classes::hkpConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpConstraintChainData(class) => class.deps_indexes(),
            Classes::hkpConstraintChainInstance(class) => class.deps_indexes(),
            Classes::hkpConstraintChainInstanceAction(class) => class.deps_indexes(),
            Classes::hkpConstraintCollisionFilter(class) => class.deps_indexes(),
            Classes::hkpConstraintData(class) => class.deps_indexes(),
            Classes::hkpConstraintInstance(class) => class.deps_indexes(),
            Classes::hkpConstraintInstanceSmallArraySerializeOverrideType(class) => {
                class.deps_indexes()
            }
            Classes::hkpConstraintMotor(class) => class.deps_indexes(),
            Classes::hkpConvexListFilter(class) => class.deps_indexes(),
            Classes::hkpConvexListShape(class) => class.deps_indexes(),
            Classes::hkpConvexPieceMeshShape(class) => class.deps_indexes(),
            Classes::hkpConvexPieceStreamData(class) => class.deps_indexes(),
            Classes::hkpConvexShape(class) => class.deps_indexes(),
            Classes::hkpConvexTransformShape(class) => class.deps_indexes(),
            Classes::hkpConvexTransformShapeBase(class) => class.deps_indexes(),
            Classes::hkpConvexTranslateShape(class) => class.deps_indexes(),
            Classes::hkpConvexVerticesConnectivity(class) => class.deps_indexes(),
            Classes::hkpConvexVerticesShape(class) => class.deps_indexes(),
            Classes::hkpConvexVerticesShapeFourVectors(class) => class.deps_indexes(),
            Classes::hkpCylinderShape(class) => class.deps_indexes(),
            Classes::hkpDashpotAction(class) => class.deps_indexes(),
            Classes::hkpDefaultConvexListFilter(class) => class.deps_indexes(),
            Classes::hkpDefaultWorldMemoryWatchDog(class) => class.deps_indexes(),
            Classes::hkpDisableEntityCollisionFilter(class) => class.deps_indexes(),
            Classes::hkpDisplayBindingData(class) => class.deps_indexes(),
            Classes::hkpDisplayBindingDataPhysicsSystem(class) => class.deps_indexes(),
            Classes::hkpDisplayBindingDataRigidBody(class) => class.deps_indexes(),
            Classes::hkpEntity(class) => class.deps_indexes(),
            Classes::hkpEntityExtendedListeners(class) => class.deps_indexes(),
            Classes::hkpEntitySmallArraySerializeOverrideType(class) => {
                class.deps_indexes()
            }
            Classes::hkpEntitySpuCollisionCallback(class) => class.deps_indexes(),
            Classes::hkpExtendedMeshShape(class) => class.deps_indexes(),
            Classes::hkpExtendedMeshShapeShapesSubpart(class) => class.deps_indexes(),
            Classes::hkpExtendedMeshShapeSubpart(class) => class.deps_indexes(),
            Classes::hkpExtendedMeshShapeTrianglesSubpart(class) => class.deps_indexes(),
            Classes::hkpFastMeshShape(class) => class.deps_indexes(),
            Classes::hkpFirstPersonGun(class) => class.deps_indexes(),
            Classes::hkpFixedRigidMotion(class) => class.deps_indexes(),
            Classes::hkpGenericConstraintData(class) => class.deps_indexes(),
            Classes::hkpGenericConstraintDataScheme(class) => class.deps_indexes(),
            Classes::hkpGenericConstraintDataSchemeConstraintInfo(class) => {
                class.deps_indexes()
            }
            Classes::hkpGravityGun(class) => class.deps_indexes(),
            Classes::hkpGroupCollisionFilter(class) => class.deps_indexes(),
            Classes::hkpGroupFilter(class) => class.deps_indexes(),
            Classes::hkpHeightFieldShape(class) => class.deps_indexes(),
            Classes::hkpHingeConstraintData(class) => class.deps_indexes(),
            Classes::hkpHingeConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpHingeLimitsData(class) => class.deps_indexes(),
            Classes::hkpHingeLimitsDataAtoms(class) => class.deps_indexes(),
            Classes::hkpIgnoreModifierConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpKeyframedRigidMotion(class) => class.deps_indexes(),
            Classes::hkpLimitedForceConstraintMotor(class) => class.deps_indexes(),
            Classes::hkpLimitedHingeConstraintData(class) => class.deps_indexes(),
            Classes::hkpLimitedHingeConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpLinConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpLinearParametricCurve(class) => class.deps_indexes(),
            Classes::hkpLinFrictionConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpLinkedCollidable(class) => class.deps_indexes(),
            Classes::hkpLinLimitConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpLinMotorConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpLinSoftConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpListShape(class) => class.deps_indexes(),
            Classes::hkpListShapeChildInfo(class) => class.deps_indexes(),
            Classes::hkpMalleableConstraintData(class) => class.deps_indexes(),
            Classes::hkpMassChangerModifierConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpMassProperties(class) => class.deps_indexes(),
            Classes::hkpMaterial(class) => class.deps_indexes(),
            Classes::hkpMaxSizeMotion(class) => class.deps_indexes(),
            Classes::hkpMeshMaterial(class) => class.deps_indexes(),
            Classes::hkpMeshShape(class) => class.deps_indexes(),
            Classes::hkpMeshShapeSubpart(class) => class.deps_indexes(),
            Classes::hkpModifierConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpMoppBvTreeShape(class) => class.deps_indexes(),
            Classes::hkpMoppCode(class) => class.deps_indexes(),
            Classes::hkpMoppCodeCodeInfo(class) => class.deps_indexes(),
            Classes::hkpMoppCodeReindexedTerminal(class) => class.deps_indexes(),
            Classes::hkpMotion(class) => class.deps_indexes(),
            Classes::hkpMotorAction(class) => class.deps_indexes(),
            Classes::hkpMountedBallGun(class) => class.deps_indexes(),
            Classes::hkpMouseSpringAction(class) => class.deps_indexes(),
            Classes::hkpMovingSurfaceModifierConstraintAtom(class) => {
                class.deps_indexes()
            }
            Classes::hkpMultiRayShape(class) => class.deps_indexes(),
            Classes::hkpMultiRayShapeRay(class) => class.deps_indexes(),
            Classes::hkpMultiSphereShape(class) => class.deps_indexes(),
            Classes::hkpMultithreadedVehicleManager(class) => class.deps_indexes(),
            Classes::hkpNamedMeshMaterial(class) => class.deps_indexes(),
            Classes::hkpNullCollisionFilter(class) => class.deps_indexes(),
            Classes::hkPostFinishAttribute(class) => class.deps_indexes(),
            Classes::hkpOverwritePivotConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpPairCollisionFilter(class) => class.deps_indexes(),
            Classes::hkpPairCollisionFilterMapPairFilterKeyOverrideType(class) => {
                class.deps_indexes()
            }
            Classes::hkpParametricCurve(class) => class.deps_indexes(),
            Classes::hkpPhantom(class) => class.deps_indexes(),
            Classes::hkpPhantomCallbackShape(class) => class.deps_indexes(),
            Classes::hkpPhysicsData(class) => class.deps_indexes(),
            Classes::hkpPhysicsSystem(class) => class.deps_indexes(),
            Classes::hkpPhysicsSystemWithContacts(class) => class.deps_indexes(),
            Classes::hkpPlaneShape(class) => class.deps_indexes(),
            Classes::hkpPointToPathConstraintData(class) => class.deps_indexes(),
            Classes::hkpPointToPlaneConstraintData(class) => class.deps_indexes(),
            Classes::hkpPointToPlaneConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpPositionConstraintMotor(class) => class.deps_indexes(),
            Classes::hkpPoweredChainData(class) => class.deps_indexes(),
            Classes::hkpPoweredChainDataConstraintInfo(class) => class.deps_indexes(),
            Classes::hkpPoweredChainMapper(class) => class.deps_indexes(),
            Classes::hkpPoweredChainMapperLinkInfo(class) => class.deps_indexes(),
            Classes::hkpPoweredChainMapperTarget(class) => class.deps_indexes(),
            Classes::hkpPrismaticConstraintData(class) => class.deps_indexes(),
            Classes::hkpPrismaticConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpProjectileGun(class) => class.deps_indexes(),
            Classes::hkpProperty(class) => class.deps_indexes(),
            Classes::hkpPropertyValue(class) => class.deps_indexes(),
            Classes::hkpPulleyConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpPulleyConstraintData(class) => class.deps_indexes(),
            Classes::hkpPulleyConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpRackAndPinionConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpRackAndPinionConstraintData(class) => class.deps_indexes(),
            Classes::hkpRackAndPinionConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpRagdollConstraintData(class) => class.deps_indexes(),
            Classes::hkpRagdollConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpRagdollLimitsData(class) => class.deps_indexes(),
            Classes::hkpRagdollLimitsDataAtoms(class) => class.deps_indexes(),
            Classes::hkpRagdollMotorConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpRayCollidableFilter(class) => class.deps_indexes(),
            Classes::hkpRayShapeCollectionFilter(class) => class.deps_indexes(),
            Classes::hkpRejectChassisListener(class) => class.deps_indexes(),
            Classes::hkpRemoveTerminalsMoppModifier(class) => class.deps_indexes(),
            Classes::hkpReorientAction(class) => class.deps_indexes(),
            Classes::hkpRigidBody(class) => class.deps_indexes(),
            Classes::hkpRotationalConstraintData(class) => class.deps_indexes(),
            Classes::hkpRotationalConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpSampledHeightFieldShape(class) => class.deps_indexes(),
            Classes::hkpSerializedAgentNnEntry(class) => class.deps_indexes(),
            Classes::hkpSerializedDisplayMarker(class) => class.deps_indexes(),
            Classes::hkpSerializedDisplayMarkerList(class) => class.deps_indexes(),
            Classes::hkpSerializedDisplayRbTransforms(class) => class.deps_indexes(),
            Classes::hkpSerializedDisplayRbTransformsDisplayTransformPair(class) => {
                class.deps_indexes()
            }
            Classes::hkpSerializedSubTrack1nInfo(class) => class.deps_indexes(),
            Classes::hkpSerializedTrack1nInfo(class) => class.deps_indexes(),
            Classes::hkpSetLocalRotationsConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpSetLocalTransformsConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpSetLocalTranslationsConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpSetupStabilizationAtom(class) => class.deps_indexes(),
            Classes::hkpShape(class) => class.deps_indexes(),
            Classes::hkpShapeCollection(class) => class.deps_indexes(),
            Classes::hkpShapeCollectionFilter(class) => class.deps_indexes(),
            Classes::hkpShapeContainer(class) => class.deps_indexes(),
            Classes::hkpShapeInfo(class) => class.deps_indexes(),
            Classes::hkpShapeModifier(class) => class.deps_indexes(),
            Classes::hkpShapePhantom(class) => class.deps_indexes(),
            Classes::hkpSimpleContactConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpSimpleContactConstraintDataInfo(class) => class.deps_indexes(),
            Classes::hkpSimpleMeshShape(class) => class.deps_indexes(),
            Classes::hkpSimpleMeshShapeTriangle(class) => class.deps_indexes(),
            Classes::hkpSimpleShapePhantom(class) => class.deps_indexes(),
            Classes::hkpSimpleShapePhantomCollisionDetail(class) => class.deps_indexes(),
            Classes::hkpSimulation(class) => class.deps_indexes(),
            Classes::hkpSingleShapeContainer(class) => class.deps_indexes(),
            Classes::hkpSoftContactModifierConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpSphereMotion(class) => class.deps_indexes(),
            Classes::hkpSphereRepShape(class) => class.deps_indexes(),
            Classes::hkpSphereShape(class) => class.deps_indexes(),
            Classes::hkpSpringAction(class) => class.deps_indexes(),
            Classes::hkpSpringDamperConstraintMotor(class) => class.deps_indexes(),
            Classes::hkpStiffSpringChainData(class) => class.deps_indexes(),
            Classes::hkpStiffSpringChainDataConstraintInfo(class) => class.deps_indexes(),
            Classes::hkpStiffSpringConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpStiffSpringConstraintData(class) => class.deps_indexes(),
            Classes::hkpStiffSpringConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpStorageExtendedMeshShape(class) => class.deps_indexes(),
            Classes::hkpStorageExtendedMeshShapeMaterial(class) => class.deps_indexes(),
            Classes::hkpStorageExtendedMeshShapeMeshSubpartStorage(class) => {
                class.deps_indexes()
            }
            Classes::hkpStorageExtendedMeshShapeShapeSubpartStorage(class) => {
                class.deps_indexes()
            }
            Classes::hkpStorageMeshShape(class) => class.deps_indexes(),
            Classes::hkpStorageMeshShapeSubpartStorage(class) => class.deps_indexes(),
            Classes::hkpStorageSampledHeightFieldShape(class) => class.deps_indexes(),
            Classes::hkpThinBoxMotion(class) => class.deps_indexes(),
            Classes::hkpTransformShape(class) => class.deps_indexes(),
            Classes::hkpTriangleShape(class) => class.deps_indexes(),
            Classes::hkpTriggerVolume(class) => class.deps_indexes(),
            Classes::hkpTriggerVolumeEventInfo(class) => class.deps_indexes(),
            Classes::hkpTriSampledHeightFieldBvTreeShape(class) => class.deps_indexes(),
            Classes::hkpTriSampledHeightFieldCollection(class) => class.deps_indexes(),
            Classes::hkpTwistLimitConstraintAtom(class) => class.deps_indexes(),
            Classes::hkpTypedBroadPhaseHandle(class) => class.deps_indexes(),
            Classes::hkpTyremarkPoint(class) => class.deps_indexes(),
            Classes::hkpTyremarksInfo(class) => class.deps_indexes(),
            Classes::hkpTyremarksWheel(class) => class.deps_indexes(),
            Classes::hkpUnaryAction(class) => class.deps_indexes(),
            Classes::hkpVehicleAerodynamics(class) => class.deps_indexes(),
            Classes::hkpVehicleBrake(class) => class.deps_indexes(),
            Classes::hkpVehicleCastBatchingManager(class) => class.deps_indexes(),
            Classes::hkpVehicleData(class) => class.deps_indexes(),
            Classes::hkpVehicleDataWheelComponentParams(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultAerodynamics(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultAnalogDriverInput(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultBrake(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultBrakeWheelBrakingProperties(class) => {
                class.deps_indexes()
            }
            Classes::hkpVehicleDefaultEngine(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultSteering(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultSuspension(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters(
                class,
            ) => class.deps_indexes(),
            Classes::hkpVehicleDefaultTransmission(class) => class.deps_indexes(),
            Classes::hkpVehicleDefaultVelocityDamper(class) => class.deps_indexes(),
            Classes::hkpVehicleDriverInput(class) => class.deps_indexes(),
            Classes::hkpVehicleDriverInputAnalogStatus(class) => class.deps_indexes(),
            Classes::hkpVehicleDriverInputStatus(class) => class.deps_indexes(),
            Classes::hkpVehicleEngine(class) => class.deps_indexes(),
            Classes::hkpVehicleFrictionDescription(class) => class.deps_indexes(),
            Classes::hkpVehicleFrictionDescriptionAxisDescription(class) => {
                class.deps_indexes()
            }
            Classes::hkpVehicleFrictionStatus(class) => class.deps_indexes(),
            Classes::hkpVehicleFrictionStatusAxisStatus(class) => class.deps_indexes(),
            Classes::hkpVehicleInstance(class) => class.deps_indexes(),
            Classes::hkpVehicleInstanceWheelInfo(class) => class.deps_indexes(),
            Classes::hkpVehicleLinearCastBatchingManager(class) => class.deps_indexes(),
            Classes::hkpVehicleLinearCastWheelCollide(class) => class.deps_indexes(),
            Classes::hkpVehicleLinearCastWheelCollideWheelState(class) => {
                class.deps_indexes()
            }
            Classes::hkpVehicleManager(class) => class.deps_indexes(),
            Classes::hkpVehicleRayCastBatchingManager(class) => class.deps_indexes(),
            Classes::hkpVehicleRayCastWheelCollide(class) => class.deps_indexes(),
            Classes::hkpVehicleSteering(class) => class.deps_indexes(),
            Classes::hkpVehicleSuspension(class) => class.deps_indexes(),
            Classes::hkpVehicleSuspensionSuspensionWheelParameters(class) => {
                class.deps_indexes()
            }
            Classes::hkpVehicleTransmission(class) => class.deps_indexes(),
            Classes::hkpVehicleVelocityDamper(class) => class.deps_indexes(),
            Classes::hkpVehicleWheelCollide(class) => class.deps_indexes(),
            Classes::hkpVelocityConstraintMotor(class) => class.deps_indexes(),
            Classes::hkpViscousSurfaceModifierConstraintAtom(class) => {
                class.deps_indexes()
            }
            Classes::hkpWeldingUtility(class) => class.deps_indexes(),
            Classes::hkpWheelConstraintData(class) => class.deps_indexes(),
            Classes::hkpWheelConstraintDataAtoms(class) => class.deps_indexes(),
            Classes::hkpWorld(class) => class.deps_indexes(),
            Classes::hkpWorldCinfo(class) => class.deps_indexes(),
            Classes::hkpWorldObject(class) => class.deps_indexes(),
            Classes::hkQTransform(class) => class.deps_indexes(),
            Classes::hkRangeInt32Attribute(class) => class.deps_indexes(),
            Classes::hkRangeRealAttribute(class) => class.deps_indexes(),
            Classes::hkReferencedObject(class) => class.deps_indexes(),
            Classes::hkReflectedFileAttribute(class) => class.deps_indexes(),
            Classes::hkResourceBase(class) => class.deps_indexes(),
            Classes::hkResourceContainer(class) => class.deps_indexes(),
            Classes::hkResourceHandle(class) => class.deps_indexes(),
            Classes::hkRootLevelContainer(class) => class.deps_indexes(),
            Classes::hkRootLevelContainerNamedVariant(class) => class.deps_indexes(),
            Classes::hkSemanticsAttribute(class) => class.deps_indexes(),
            Classes::hkSimpleLocalFrame(class) => class.deps_indexes(),
            Classes::hkSphere(class) => class.deps_indexes(),
            Classes::hkSweptTransform(class) => class.deps_indexes(),
            Classes::hkTraceStreamTitle(class) => class.deps_indexes(),
            Classes::hkTrackerSerializableScanSnapshot(class) => class.deps_indexes(),
            Classes::hkTrackerSerializableScanSnapshotAllocation(class) => {
                class.deps_indexes()
            }
            Classes::hkTrackerSerializableScanSnapshotBlock(class) => {
                class.deps_indexes()
            }
            Classes::hkUiAttribute(class) => class.deps_indexes(),
            Classes::hkVertexFormat(class) => class.deps_indexes(),
            Classes::hkVertexFormatElement(class) => class.deps_indexes(),
            Classes::hkWorldMemoryAvailableWatchDog(class) => class.deps_indexes(),
            Classes::hkxAnimatedFloat(class) => class.deps_indexes(),
            Classes::hkxAnimatedMatrix(class) => class.deps_indexes(),
            Classes::hkxAnimatedQuaternion(class) => class.deps_indexes(),
            Classes::hkxAnimatedVector(class) => class.deps_indexes(),
            Classes::hkxAttribute(class) => class.deps_indexes(),
            Classes::hkxAttributeGroup(class) => class.deps_indexes(),
            Classes::hkxAttributeHolder(class) => class.deps_indexes(),
            Classes::hkxCamera(class) => class.deps_indexes(),
            Classes::hkxEdgeSelectionChannel(class) => class.deps_indexes(),
            Classes::hkxEnum(class) => class.deps_indexes(),
            Classes::hkxEnumItem(class) => class.deps_indexes(),
            Classes::hkxEnvironment(class) => class.deps_indexes(),
            Classes::hkxEnvironmentVariable(class) => class.deps_indexes(),
            Classes::hkxIndexBuffer(class) => class.deps_indexes(),
            Classes::hkxLight(class) => class.deps_indexes(),
            Classes::hkxMaterial(class) => class.deps_indexes(),
            Classes::hkxMaterialEffect(class) => class.deps_indexes(),
            Classes::hkxMaterialProperty(class) => class.deps_indexes(),
            Classes::hkxMaterialShader(class) => class.deps_indexes(),
            Classes::hkxMaterialShaderSet(class) => class.deps_indexes(),
            Classes::hkxMaterialTextureStage(class) => class.deps_indexes(),
            Classes::hkxMesh(class) => class.deps_indexes(),
            Classes::hkxMeshSection(class) => class.deps_indexes(),
            Classes::hkxMeshUserChannelInfo(class) => class.deps_indexes(),
            Classes::hkxNode(class) => class.deps_indexes(),
            Classes::hkxNodeAnnotationData(class) => class.deps_indexes(),
            Classes::hkxNodeSelectionSet(class) => class.deps_indexes(),
            Classes::hkxScene(class) => class.deps_indexes(),
            Classes::hkxSkinBinding(class) => class.deps_indexes(),
            Classes::hkxSparselyAnimatedBool(class) => class.deps_indexes(),
            Classes::hkxSparselyAnimatedEnum(class) => class.deps_indexes(),
            Classes::hkxSparselyAnimatedInt(class) => class.deps_indexes(),
            Classes::hkxSparselyAnimatedString(class) => class.deps_indexes(),
            Classes::hkxTextureFile(class) => class.deps_indexes(),
            Classes::hkxTextureInplace(class) => class.deps_indexes(),
            Classes::hkxTriangleSelectionChannel(class) => class.deps_indexes(),
            Classes::hkxVertexBuffer(class) => class.deps_indexes(),
            Classes::hkxVertexBufferVertexData(class) => class.deps_indexes(),
            Classes::hkxVertexDescription(class) => class.deps_indexes(),
            Classes::hkxVertexDescriptionElementDecl(class) => class.deps_indexes(),
            Classes::hkxVertexFloatDataChannel(class) => class.deps_indexes(),
            Classes::hkxVertexIntDataChannel(class) => class.deps_indexes(),
            Classes::hkxVertexSelectionChannel(class) => class.deps_indexes(),
            Classes::hkxVertexVectorDataChannel(class) => class.deps_indexes(),
        }
    }
}
impl<'a> _serde::Serialize for Classes<'a> {
    fn serialize<S: _serde::ser::Serializer>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        match self {
            Classes::BGSGamebryoSequenceGenerator(class) => class.serialize(serializer),
            Classes::BSBoneSwitchGenerator(class) => class.serialize(serializer),
            Classes::BSBoneSwitchGeneratorBoneData(class) => class.serialize(serializer),
            Classes::BSComputeAddBoneAnimModifier(class) => class.serialize(serializer),
            Classes::BSCyclicBlendTransitionGenerator(class) => {
                class.serialize(serializer)
            }
            Classes::BSDecomposeVectorModifier(class) => class.serialize(serializer),
            Classes::BSDirectAtModifier(class) => class.serialize(serializer),
            Classes::BSDistTriggerModifier(class) => class.serialize(serializer),
            Classes::BSEventEveryNEventsModifier(class) => class.serialize(serializer),
            Classes::BSEventOnDeactivateModifier(class) => class.serialize(serializer),
            Classes::BSEventOnFalseToTrueModifier(class) => class.serialize(serializer),
            Classes::BSGetTimeStepModifier(class) => class.serialize(serializer),
            Classes::BSInterpValueModifier(class) => class.serialize(serializer),
            Classes::BSIsActiveModifier(class) => class.serialize(serializer),
            Classes::BSIStateManagerModifier(class) => class.serialize(serializer),
            Classes::BSIStateManagerModifierBSiStateData(class) => {
                class.serialize(serializer)
            }
            Classes::BSIStateManagerModifierBSIStateManagerStateListener(class) => {
                class.serialize(serializer)
            }
            Classes::BSiStateTaggingGenerator(class) => class.serialize(serializer),
            Classes::BSLimbIKModifier(class) => class.serialize(serializer),
            Classes::BSLookAtModifier(class) => class.serialize(serializer),
            Classes::BSLookAtModifierBoneData(class) => class.serialize(serializer),
            Classes::BSModifyOnceModifier(class) => class.serialize(serializer),
            Classes::BSOffsetAnimationGenerator(class) => class.serialize(serializer),
            Classes::BSPassByTargetTriggerModifier(class) => class.serialize(serializer),
            Classes::BSRagdollContactListenerModifier(class) => {
                class.serialize(serializer)
            }
            Classes::BSSpeedSamplerModifier(class) => class.serialize(serializer),
            Classes::BSSynchronizedClipGenerator(class) => class.serialize(serializer),
            Classes::BSTimerModifier(class) => class.serialize(serializer),
            Classes::BSTweenerModifier(class) => class.serialize(serializer),
            Classes::hkAabb(class) => class.serialize(serializer),
            Classes::hkAabbHalf(class) => class.serialize(serializer),
            Classes::hkAabbUint32(class) => class.serialize(serializer),
            Classes::hkaAnimatedReferenceFrame(class) => class.serialize(serializer),
            Classes::hkaAnimation(class) => class.serialize(serializer),
            Classes::hkaAnimationBinding(class) => class.serialize(serializer),
            Classes::hkaAnimationContainer(class) => class.serialize(serializer),
            Classes::hkaAnimationPreviewColorContainer(class) => {
                class.serialize(serializer)
            }
            Classes::hkaAnnotationTrack(class) => class.serialize(serializer),
            Classes::hkaAnnotationTrackAnnotation(class) => class.serialize(serializer),
            Classes::hkaBone(class) => class.serialize(serializer),
            Classes::hkaBoneAttachment(class) => class.serialize(serializer),
            Classes::hkaDefaultAnimatedReferenceFrame(class) => {
                class.serialize(serializer)
            }
            Classes::hkaDeltaCompressedAnimation(class) => class.serialize(serializer),
            Classes::hkaDeltaCompressedAnimationQuantizationFormat(class) => {
                class.serialize(serializer)
            }
            Classes::hkaFootstepAnalysisInfo(class) => class.serialize(serializer),
            Classes::hkaFootstepAnalysisInfoContainer(class) => {
                class.serialize(serializer)
            }
            Classes::hkaInterleavedUncompressedAnimation(class) => {
                class.serialize(serializer)
            }
            Classes::hkaKeyFrameHierarchyUtility(class) => class.serialize(serializer),
            Classes::hkaKeyFrameHierarchyUtilityControlData(class) => {
                class.serialize(serializer)
            }
            Classes::hkAlignSceneToNodeOptions(class) => class.serialize(serializer),
            Classes::hkaMeshBinding(class) => class.serialize(serializer),
            Classes::hkaMeshBindingMapping(class) => class.serialize(serializer),
            Classes::hkaQuantizedAnimation(class) => class.serialize(serializer),
            Classes::hkaQuantizedAnimationTrackCompressionParams(class) => {
                class.serialize(serializer)
            }
            Classes::hkaRagdollInstance(class) => class.serialize(serializer),
            Classes::hkArrayTypeAttribute(class) => class.serialize(serializer),
            Classes::hkaSkeleton(class) => class.serialize(serializer),
            Classes::hkaSkeletonLocalFrameOnBone(class) => class.serialize(serializer),
            Classes::hkaSkeletonMapper(class) => class.serialize(serializer),
            Classes::hkaSkeletonMapperData(class) => class.serialize(serializer),
            Classes::hkaSkeletonMapperDataChainMapping(class) => {
                class.serialize(serializer)
            }
            Classes::hkaSkeletonMapperDataSimpleMapping(class) => {
                class.serialize(serializer)
            }
            Classes::hkaSplineCompressedAnimation(class) => class.serialize(serializer),
            Classes::hkaSplineCompressedAnimationAnimationCompressionParams(class) => {
                class.serialize(serializer)
            }
            Classes::hkaSplineCompressedAnimationTrackCompressionParams(class) => {
                class.serialize(serializer)
            }
            Classes::hkaWaveletCompressedAnimation(class) => class.serialize(serializer),
            Classes::hkaWaveletCompressedAnimationCompressionParams(class) => {
                class.serialize(serializer)
            }
            Classes::hkaWaveletCompressedAnimationQuantizationFormat(class) => {
                class.serialize(serializer)
            }
            Classes::hkBaseObject(class) => class.serialize(serializer),
            Classes::hkbAttachmentModifier(class) => class.serialize(serializer),
            Classes::hkbAttachmentSetup(class) => class.serialize(serializer),
            Classes::hkbAttributeModifier(class) => class.serialize(serializer),
            Classes::hkbAttributeModifierAssignment(class) => class.serialize(serializer),
            Classes::hkbAuxiliaryNodeInfo(class) => class.serialize(serializer),
            Classes::hkbBehaviorEventsInfo(class) => class.serialize(serializer),
            Classes::hkbBehaviorGraph(class) => class.serialize(serializer),
            Classes::hkbBehaviorGraphData(class) => class.serialize(serializer),
            Classes::hkbBehaviorGraphInternalState(class) => class.serialize(serializer),
            Classes::hkbBehaviorGraphInternalStateInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkbBehaviorGraphStringData(class) => class.serialize(serializer),
            Classes::hkbBehaviorInfo(class) => class.serialize(serializer),
            Classes::hkbBehaviorInfoIdToNamePair(class) => class.serialize(serializer),
            Classes::hkbBehaviorReferenceGenerator(class) => class.serialize(serializer),
            Classes::hkbBindable(class) => class.serialize(serializer),
            Classes::hkbBlendCurveUtils(class) => class.serialize(serializer),
            Classes::hkbBlenderGenerator(class) => class.serialize(serializer),
            Classes::hkbBlenderGeneratorChild(class) => class.serialize(serializer),
            Classes::hkbBlenderGeneratorChildInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbBlenderGeneratorInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbBlendingTransitionEffect(class) => class.serialize(serializer),
            Classes::hkbBlendingTransitionEffectInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbBoneIndexArray(class) => class.serialize(serializer),
            Classes::hkbBoneWeightArray(class) => class.serialize(serializer),
            Classes::hkbBoolVariableSequencedData(class) => class.serialize(serializer),
            Classes::hkbBoolVariableSequencedDataSample(class) => {
                class.serialize(serializer)
            }
            Classes::hkbCameraShakeEventPayload(class) => class.serialize(serializer),
            Classes::hkbCharacter(class) => class.serialize(serializer),
            Classes::hkbCharacterAddedInfo(class) => class.serialize(serializer),
            Classes::hkbCharacterControlCommand(class) => class.serialize(serializer),
            Classes::hkbCharacterControllerControlData(class) => {
                class.serialize(serializer)
            }
            Classes::hkbCharacterControllerModifier(class) => class.serialize(serializer),
            Classes::hkbCharacterControllerModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbCharacterData(class) => class.serialize(serializer),
            Classes::hkbCharacterDataCharacterControllerInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkbCharacterInfo(class) => class.serialize(serializer),
            Classes::hkbCharacterSetup(class) => class.serialize(serializer),
            Classes::hkbCharacterSkinInfo(class) => class.serialize(serializer),
            Classes::hkbCharacterSteppedInfo(class) => class.serialize(serializer),
            Classes::hkbCharacterStringData(class) => class.serialize(serializer),
            Classes::hkbClientCharacterState(class) => class.serialize(serializer),
            Classes::hkbClipGenerator(class) => class.serialize(serializer),
            Classes::hkbClipGeneratorEcho(class) => class.serialize(serializer),
            Classes::hkbClipGeneratorInternalState(class) => class.serialize(serializer),
            Classes::hkbClipTrigger(class) => class.serialize(serializer),
            Classes::hkbClipTriggerArray(class) => class.serialize(serializer),
            Classes::hkbCombineTransformsModifier(class) => class.serialize(serializer),
            Classes::hkbCombineTransformsModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbCompiledExpressionSet(class) => class.serialize(serializer),
            Classes::hkbCompiledExpressionSetToken(class) => class.serialize(serializer),
            Classes::hkbComputeDirectionModifier(class) => class.serialize(serializer),
            Classes::hkbComputeDirectionModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbComputeRotationFromAxisAngleModifier(class) => {
                class.serialize(serializer)
            }
            Classes::hkbComputeRotationFromAxisAngleModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbComputeRotationToTargetModifier(class) => {
                class.serialize(serializer)
            }
            Classes::hkbComputeRotationToTargetModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbCondition(class) => class.serialize(serializer),
            Classes::hkbContext(class) => class.serialize(serializer),
            Classes::hkbDampingModifier(class) => class.serialize(serializer),
            Classes::hkbDampingModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbDefaultMessageLog(class) => class.serialize(serializer),
            Classes::hkbDelayedModifier(class) => class.serialize(serializer),
            Classes::hkbDelayedModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbDetectCloseToGroundModifier(class) => class.serialize(serializer),
            Classes::hkbDetectCloseToGroundModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbEvaluateExpressionModifier(class) => class.serialize(serializer),
            Classes::hkbEvaluateExpressionModifierInternalExpressionData(class) => {
                class.serialize(serializer)
            }
            Classes::hkbEvaluateExpressionModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbEvaluateHandleModifier(class) => class.serialize(serializer),
            Classes::hkbEvent(class) => class.serialize(serializer),
            Classes::hkbEventBase(class) => class.serialize(serializer),
            Classes::hkbEventDrivenModifier(class) => class.serialize(serializer),
            Classes::hkbEventDrivenModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbEventInfo(class) => class.serialize(serializer),
            Classes::hkbEventPayload(class) => class.serialize(serializer),
            Classes::hkbEventPayloadList(class) => class.serialize(serializer),
            Classes::hkbEventProperty(class) => class.serialize(serializer),
            Classes::hkbEventRaisedInfo(class) => class.serialize(serializer),
            Classes::hkbEventRangeData(class) => class.serialize(serializer),
            Classes::hkbEventRangeDataArray(class) => class.serialize(serializer),
            Classes::hkbEventSequencedData(class) => class.serialize(serializer),
            Classes::hkbEventSequencedDataSequencedEvent(class) => {
                class.serialize(serializer)
            }
            Classes::hkbEventsFromRangeModifier(class) => class.serialize(serializer),
            Classes::hkbEventsFromRangeModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbExpressionCondition(class) => class.serialize(serializer),
            Classes::hkbExpressionData(class) => class.serialize(serializer),
            Classes::hkbExpressionDataArray(class) => class.serialize(serializer),
            Classes::hkbExtractRagdollPoseModifier(class) => class.serialize(serializer),
            Classes::hkbFootIkControlData(class) => class.serialize(serializer),
            Classes::hkbFootIkControlsModifier(class) => class.serialize(serializer),
            Classes::hkbFootIkControlsModifierLeg(class) => class.serialize(serializer),
            Classes::hkbFootIkDriverInfo(class) => class.serialize(serializer),
            Classes::hkbFootIkDriverInfoLeg(class) => class.serialize(serializer),
            Classes::hkbFootIkGains(class) => class.serialize(serializer),
            Classes::hkbFootIkModifier(class) => class.serialize(serializer),
            Classes::hkbFootIkModifierInternalLegData(class) => {
                class.serialize(serializer)
            }
            Classes::hkbFootIkModifierLeg(class) => class.serialize(serializer),
            Classes::hkbGenerator(class) => class.serialize(serializer),
            Classes::hkbGeneratorOutputListener(class) => class.serialize(serializer),
            Classes::hkbGeneratorSyncInfo(class) => class.serialize(serializer),
            Classes::hkbGeneratorSyncInfoSyncPoint(class) => class.serialize(serializer),
            Classes::hkbGeneratorTransitionEffect(class) => class.serialize(serializer),
            Classes::hkbGeneratorTransitionEffectInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbGetHandleOnBoneModifier(class) => class.serialize(serializer),
            Classes::hkbGetUpModifier(class) => class.serialize(serializer),
            Classes::hkbGetUpModifierInternalState(class) => class.serialize(serializer),
            Classes::hkbGetWorldFromModelModifier(class) => class.serialize(serializer),
            Classes::hkbGetWorldFromModelModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbHandIkControlData(class) => class.serialize(serializer),
            Classes::hkbHandIkControlsModifier(class) => class.serialize(serializer),
            Classes::hkbHandIkControlsModifierHand(class) => class.serialize(serializer),
            Classes::hkbHandIkDriverInfo(class) => class.serialize(serializer),
            Classes::hkbHandIkDriverInfoHand(class) => class.serialize(serializer),
            Classes::hkbHandIkModifier(class) => class.serialize(serializer),
            Classes::hkbHandIkModifierHand(class) => class.serialize(serializer),
            Classes::hkbHandle(class) => class.serialize(serializer),
            Classes::hkbIntEventPayload(class) => class.serialize(serializer),
            Classes::hkbIntVariableSequencedData(class) => class.serialize(serializer),
            Classes::hkbIntVariableSequencedDataSample(class) => {
                class.serialize(serializer)
            }
            Classes::hkBitField(class) => class.serialize(serializer),
            Classes::hkbKeyframeBonesModifier(class) => class.serialize(serializer),
            Classes::hkbKeyframeBonesModifierKeyframeInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkbLinkedSymbolInfo(class) => class.serialize(serializer),
            Classes::hkbLookAtModifier(class) => class.serialize(serializer),
            Classes::hkbLookAtModifierInternalState(class) => class.serialize(serializer),
            Classes::hkbManualSelectorGenerator(class) => class.serialize(serializer),
            Classes::hkbManualSelectorGeneratorInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbMessageLog(class) => class.serialize(serializer),
            Classes::hkbMirroredSkeletonInfo(class) => class.serialize(serializer),
            Classes::hkbMirrorModifier(class) => class.serialize(serializer),
            Classes::hkbModifier(class) => class.serialize(serializer),
            Classes::hkbModifierGenerator(class) => class.serialize(serializer),
            Classes::hkbModifierList(class) => class.serialize(serializer),
            Classes::hkbModifierWrapper(class) => class.serialize(serializer),
            Classes::hkbMoveCharacterModifier(class) => class.serialize(serializer),
            Classes::hkbMoveCharacterModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbNamedEventPayload(class) => class.serialize(serializer),
            Classes::hkbNamedIntEventPayload(class) => class.serialize(serializer),
            Classes::hkbNamedRealEventPayload(class) => class.serialize(serializer),
            Classes::hkbNamedStringEventPayload(class) => class.serialize(serializer),
            Classes::hkbNode(class) => class.serialize(serializer),
            Classes::hkbNodeInternalStateInfo(class) => class.serialize(serializer),
            Classes::hkbParticleSystemEventPayload(class) => class.serialize(serializer),
            Classes::hkbPoseMatchingGenerator(class) => class.serialize(serializer),
            Classes::hkbPoseMatchingGeneratorInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbPoweredRagdollControlData(class) => class.serialize(serializer),
            Classes::hkbPoweredRagdollControlsModifier(class) => {
                class.serialize(serializer)
            }
            Classes::hkbProjectData(class) => class.serialize(serializer),
            Classes::hkbProjectStringData(class) => class.serialize(serializer),
            Classes::hkbProxyModifier(class) => class.serialize(serializer),
            Classes::hkbProxyModifierProxyInfo(class) => class.serialize(serializer),
            Classes::hkbRaiseEventCommand(class) => class.serialize(serializer),
            Classes::hkbRealEventPayload(class) => class.serialize(serializer),
            Classes::hkbRealVariableSequencedData(class) => class.serialize(serializer),
            Classes::hkbRealVariableSequencedDataSample(class) => {
                class.serialize(serializer)
            }
            Classes::hkbReferencePoseGenerator(class) => class.serialize(serializer),
            Classes::hkbRegisteredGenerator(class) => class.serialize(serializer),
            Classes::hkbRigidBodyRagdollControlData(class) => class.serialize(serializer),
            Classes::hkbRigidBodyRagdollControlsModifier(class) => {
                class.serialize(serializer)
            }
            Classes::hkbRoleAttribute(class) => class.serialize(serializer),
            Classes::hkbRotateCharacterModifier(class) => class.serialize(serializer),
            Classes::hkbRotateCharacterModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbSenseHandleModifier(class) => class.serialize(serializer),
            Classes::hkbSenseHandleModifierRange(class) => class.serialize(serializer),
            Classes::hkbSequence(class) => class.serialize(serializer),
            Classes::hkbSequencedData(class) => class.serialize(serializer),
            Classes::hkbSequenceInternalState(class) => class.serialize(serializer),
            Classes::hkbSequenceStringData(class) => class.serialize(serializer),
            Classes::hkbSetBehaviorCommand(class) => class.serialize(serializer),
            Classes::hkbSetLocalTimeOfClipGeneratorCommand(class) => {
                class.serialize(serializer)
            }
            Classes::hkbSetNodePropertyCommand(class) => class.serialize(serializer),
            Classes::hkbSetWordVariableCommand(class) => class.serialize(serializer),
            Classes::hkbSetWorldFromModelModifier(class) => class.serialize(serializer),
            Classes::hkbSimulationControlCommand(class) => class.serialize(serializer),
            Classes::hkbSimulationStateInfo(class) => class.serialize(serializer),
            Classes::hkbStateChooser(class) => class.serialize(serializer),
            Classes::hkbStateListener(class) => class.serialize(serializer),
            Classes::hkbStateMachine(class) => class.serialize(serializer),
            Classes::hkbStateMachineActiveTransitionInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStateMachineDelayedTransitionInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStateMachineEventPropertyArray(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStateMachineInternalState(class) => class.serialize(serializer),
            Classes::hkbStateMachineNestedStateMachineData(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStateMachineProspectiveTransitionInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStateMachineStateInfo(class) => class.serialize(serializer),
            Classes::hkbStateMachineTimeInterval(class) => class.serialize(serializer),
            Classes::hkbStateMachineTransitionInfo(class) => class.serialize(serializer),
            Classes::hkbStateMachineTransitionInfoArray(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStateMachineTransitionInfoReference(class) => {
                class.serialize(serializer)
            }
            Classes::hkbStringCondition(class) => class.serialize(serializer),
            Classes::hkbStringEventPayload(class) => class.serialize(serializer),
            Classes::hkbTestStateChooser(class) => class.serialize(serializer),
            Classes::hkbTimerModifier(class) => class.serialize(serializer),
            Classes::hkbTimerModifierInternalState(class) => class.serialize(serializer),
            Classes::hkbTransformVectorModifier(class) => class.serialize(serializer),
            Classes::hkbTransformVectorModifierInternalState(class) => {
                class.serialize(serializer)
            }
            Classes::hkbTransitionEffect(class) => class.serialize(serializer),
            Classes::hkbTwistModifier(class) => class.serialize(serializer),
            Classes::hkbVariableBindingSet(class) => class.serialize(serializer),
            Classes::hkbVariableBindingSetBinding(class) => class.serialize(serializer),
            Classes::hkbVariableInfo(class) => class.serialize(serializer),
            Classes::hkbVariableValue(class) => class.serialize(serializer),
            Classes::hkbVariableValueSet(class) => class.serialize(serializer),
            Classes::hkbWorldEnums(class) => class.serialize(serializer),
            Classes::hkbWorldFromModelModeData(class) => class.serialize(serializer),
            Classes::hkClass(class) => class.serialize(serializer),
            Classes::hkClassEnum(class) => class.serialize(serializer),
            Classes::hkClassEnumItem(class) => class.serialize(serializer),
            Classes::hkClassMember(class) => class.serialize(serializer),
            Classes::hkColor(class) => class.serialize(serializer),
            Classes::hkContactPoint(class) => class.serialize(serializer),
            Classes::hkContactPointMaterial(class) => class.serialize(serializer),
            Classes::hkCustomAttributes(class) => class.serialize(serializer),
            Classes::hkCustomAttributesAttribute(class) => class.serialize(serializer),
            Classes::hkDataObjectTypeAttribute(class) => class.serialize(serializer),
            Classes::hkDescriptionAttribute(class) => class.serialize(serializer),
            Classes::hkDocumentationAttribute(class) => class.serialize(serializer),
            Classes::hkGeometry(class) => class.serialize(serializer),
            Classes::hkGeometryTriangle(class) => class.serialize(serializer),
            Classes::hkGizmoAttribute(class) => class.serialize(serializer),
            Classes::hkHalf8(class) => class.serialize(serializer),
            Classes::hkIndexedTransformSet(class) => class.serialize(serializer),
            Classes::hkLinkAttribute(class) => class.serialize(serializer),
            Classes::hkLocalFrame(class) => class.serialize(serializer),
            Classes::hkLocalFrameGroup(class) => class.serialize(serializer),
            Classes::hkMemoryMeshBody(class) => class.serialize(serializer),
            Classes::hkMemoryMeshMaterial(class) => class.serialize(serializer),
            Classes::hkMemoryMeshShape(class) => class.serialize(serializer),
            Classes::hkMemoryMeshTexture(class) => class.serialize(serializer),
            Classes::hkMemoryMeshVertexBuffer(class) => class.serialize(serializer),
            Classes::hkMemoryResourceContainer(class) => class.serialize(serializer),
            Classes::hkMemoryResourceHandle(class) => class.serialize(serializer),
            Classes::hkMemoryResourceHandleExternalLink(class) => {
                class.serialize(serializer)
            }
            Classes::hkMemoryTrackerAttribute(class) => class.serialize(serializer),
            Classes::hkMeshBody(class) => class.serialize(serializer),
            Classes::hkMeshBoneIndexMapping(class) => class.serialize(serializer),
            Classes::hkMeshMaterial(class) => class.serialize(serializer),
            Classes::hkMeshSection(class) => class.serialize(serializer),
            Classes::hkMeshSectionCinfo(class) => class.serialize(serializer),
            Classes::hkMeshShape(class) => class.serialize(serializer),
            Classes::hkMeshTexture(class) => class.serialize(serializer),
            Classes::hkMeshVertexBuffer(class) => class.serialize(serializer),
            Classes::hkModelerNodeTypeAttribute(class) => class.serialize(serializer),
            Classes::hkMonitorStreamColorTable(class) => class.serialize(serializer),
            Classes::hkMonitorStreamColorTableColorPair(class) => {
                class.serialize(serializer)
            }
            Classes::hkMonitorStreamFrameInfo(class) => class.serialize(serializer),
            Classes::hkMonitorStreamStringMap(class) => class.serialize(serializer),
            Classes::hkMonitorStreamStringMapStringMap(class) => {
                class.serialize(serializer)
            }
            Classes::hkMoppBvTreeShapeBase(class) => class.serialize(serializer),
            Classes::hkMotionState(class) => class.serialize(serializer),
            Classes::hkMultipleVertexBuffer(class) => class.serialize(serializer),
            Classes::hkMultipleVertexBufferElementInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkMultipleVertexBufferLockedElement(class) => {
                class.serialize(serializer)
            }
            Classes::hkMultipleVertexBufferVertexBufferInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkMultiThreadCheck(class) => class.serialize(serializer),
            Classes::hkp2dAngConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpAabbPhantom(class) => class.serialize(serializer),
            Classes::hkPackedVector3(class) => class.serialize(serializer),
            Classes::hkPackfileHeader(class) => class.serialize(serializer),
            Classes::hkPackfileSectionHeader(class) => class.serialize(serializer),
            Classes::hkpAction(class) => class.serialize(serializer),
            Classes::hkpAgent1nSector(class) => class.serialize(serializer),
            Classes::hkpAngConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpAngFrictionConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpAngLimitConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpAngMotorConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpAngularDashpotAction(class) => class.serialize(serializer),
            Classes::hkpArrayAction(class) => class.serialize(serializer),
            Classes::hkpBallAndSocketConstraintData(class) => class.serialize(serializer),
            Classes::hkpBallAndSocketConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpBallGun(class) => class.serialize(serializer),
            Classes::hkpBallSocketChainData(class) => class.serialize(serializer),
            Classes::hkpBallSocketChainDataConstraintInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkpBallSocketConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpBinaryAction(class) => class.serialize(serializer),
            Classes::hkpBoxMotion(class) => class.serialize(serializer),
            Classes::hkpBoxShape(class) => class.serialize(serializer),
            Classes::hkpBreakableBody(class) => class.serialize(serializer),
            Classes::hkpBreakableConstraintData(class) => class.serialize(serializer),
            Classes::hkpBridgeAtoms(class) => class.serialize(serializer),
            Classes::hkpBridgeConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpBroadPhaseHandle(class) => class.serialize(serializer),
            Classes::hkpBvShape(class) => class.serialize(serializer),
            Classes::hkpBvTreeShape(class) => class.serialize(serializer),
            Classes::hkpCachingShapePhantom(class) => class.serialize(serializer),
            Classes::hkpCallbackConstraintMotor(class) => class.serialize(serializer),
            Classes::hkpCapsuleShape(class) => class.serialize(serializer),
            Classes::hkpCdBody(class) => class.serialize(serializer),
            Classes::hkpCenterOfMassChangerModifierConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpCharacterControllerCinfo(class) => class.serialize(serializer),
            Classes::hkpCharacterMotion(class) => class.serialize(serializer),
            Classes::hkpCharacterProxyCinfo(class) => class.serialize(serializer),
            Classes::hkpCharacterRigidBodyCinfo(class) => class.serialize(serializer),
            Classes::hkpCogWheelConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpCogWheelConstraintData(class) => class.serialize(serializer),
            Classes::hkpCogWheelConstraintDataAtoms(class) => class.serialize(serializer),
            Classes::hkpCollidable(class) => class.serialize(serializer),
            Classes::hkpCollidableBoundingVolumeData(class) => {
                class.serialize(serializer)
            }
            Classes::hkpCollidableCollidableFilter(class) => class.serialize(serializer),
            Classes::hkpCollisionFilter(class) => class.serialize(serializer),
            Classes::hkpCollisionFilterList(class) => class.serialize(serializer),
            Classes::hkpCompressedMeshShape(class) => class.serialize(serializer),
            Classes::hkpCompressedMeshShapeBigTriangle(class) => {
                class.serialize(serializer)
            }
            Classes::hkpCompressedMeshShapeChunk(class) => class.serialize(serializer),
            Classes::hkpCompressedMeshShapeConvexPiece(class) => {
                class.serialize(serializer)
            }
            Classes::hkpCompressedSampledHeightFieldShape(class) => {
                class.serialize(serializer)
            }
            Classes::hkpConeLimitConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpConstrainedSystemFilter(class) => class.serialize(serializer),
            Classes::hkpConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpConstraintChainData(class) => class.serialize(serializer),
            Classes::hkpConstraintChainInstance(class) => class.serialize(serializer),
            Classes::hkpConstraintChainInstanceAction(class) => {
                class.serialize(serializer)
            }
            Classes::hkpConstraintCollisionFilter(class) => class.serialize(serializer),
            Classes::hkpConstraintData(class) => class.serialize(serializer),
            Classes::hkpConstraintInstance(class) => class.serialize(serializer),
            Classes::hkpConstraintInstanceSmallArraySerializeOverrideType(class) => {
                class.serialize(serializer)
            }
            Classes::hkpConstraintMotor(class) => class.serialize(serializer),
            Classes::hkpConvexListFilter(class) => class.serialize(serializer),
            Classes::hkpConvexListShape(class) => class.serialize(serializer),
            Classes::hkpConvexPieceMeshShape(class) => class.serialize(serializer),
            Classes::hkpConvexPieceStreamData(class) => class.serialize(serializer),
            Classes::hkpConvexShape(class) => class.serialize(serializer),
            Classes::hkpConvexTransformShape(class) => class.serialize(serializer),
            Classes::hkpConvexTransformShapeBase(class) => class.serialize(serializer),
            Classes::hkpConvexTranslateShape(class) => class.serialize(serializer),
            Classes::hkpConvexVerticesConnectivity(class) => class.serialize(serializer),
            Classes::hkpConvexVerticesShape(class) => class.serialize(serializer),
            Classes::hkpConvexVerticesShapeFourVectors(class) => {
                class.serialize(serializer)
            }
            Classes::hkpCylinderShape(class) => class.serialize(serializer),
            Classes::hkpDashpotAction(class) => class.serialize(serializer),
            Classes::hkpDefaultConvexListFilter(class) => class.serialize(serializer),
            Classes::hkpDefaultWorldMemoryWatchDog(class) => class.serialize(serializer),
            Classes::hkpDisableEntityCollisionFilter(class) => {
                class.serialize(serializer)
            }
            Classes::hkpDisplayBindingData(class) => class.serialize(serializer),
            Classes::hkpDisplayBindingDataPhysicsSystem(class) => {
                class.serialize(serializer)
            }
            Classes::hkpDisplayBindingDataRigidBody(class) => class.serialize(serializer),
            Classes::hkpEntity(class) => class.serialize(serializer),
            Classes::hkpEntityExtendedListeners(class) => class.serialize(serializer),
            Classes::hkpEntitySmallArraySerializeOverrideType(class) => {
                class.serialize(serializer)
            }
            Classes::hkpEntitySpuCollisionCallback(class) => class.serialize(serializer),
            Classes::hkpExtendedMeshShape(class) => class.serialize(serializer),
            Classes::hkpExtendedMeshShapeShapesSubpart(class) => {
                class.serialize(serializer)
            }
            Classes::hkpExtendedMeshShapeSubpart(class) => class.serialize(serializer),
            Classes::hkpExtendedMeshShapeTrianglesSubpart(class) => {
                class.serialize(serializer)
            }
            Classes::hkpFastMeshShape(class) => class.serialize(serializer),
            Classes::hkpFirstPersonGun(class) => class.serialize(serializer),
            Classes::hkpFixedRigidMotion(class) => class.serialize(serializer),
            Classes::hkpGenericConstraintData(class) => class.serialize(serializer),
            Classes::hkpGenericConstraintDataScheme(class) => class.serialize(serializer),
            Classes::hkpGenericConstraintDataSchemeConstraintInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkpGravityGun(class) => class.serialize(serializer),
            Classes::hkpGroupCollisionFilter(class) => class.serialize(serializer),
            Classes::hkpGroupFilter(class) => class.serialize(serializer),
            Classes::hkpHeightFieldShape(class) => class.serialize(serializer),
            Classes::hkpHingeConstraintData(class) => class.serialize(serializer),
            Classes::hkpHingeConstraintDataAtoms(class) => class.serialize(serializer),
            Classes::hkpHingeLimitsData(class) => class.serialize(serializer),
            Classes::hkpHingeLimitsDataAtoms(class) => class.serialize(serializer),
            Classes::hkpIgnoreModifierConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpKeyframedRigidMotion(class) => class.serialize(serializer),
            Classes::hkpLimitedForceConstraintMotor(class) => class.serialize(serializer),
            Classes::hkpLimitedHingeConstraintData(class) => class.serialize(serializer),
            Classes::hkpLimitedHingeConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpLinConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpLinearParametricCurve(class) => class.serialize(serializer),
            Classes::hkpLinFrictionConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpLinkedCollidable(class) => class.serialize(serializer),
            Classes::hkpLinLimitConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpLinMotorConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpLinSoftConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpListShape(class) => class.serialize(serializer),
            Classes::hkpListShapeChildInfo(class) => class.serialize(serializer),
            Classes::hkpMalleableConstraintData(class) => class.serialize(serializer),
            Classes::hkpMassChangerModifierConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpMassProperties(class) => class.serialize(serializer),
            Classes::hkpMaterial(class) => class.serialize(serializer),
            Classes::hkpMaxSizeMotion(class) => class.serialize(serializer),
            Classes::hkpMeshMaterial(class) => class.serialize(serializer),
            Classes::hkpMeshShape(class) => class.serialize(serializer),
            Classes::hkpMeshShapeSubpart(class) => class.serialize(serializer),
            Classes::hkpModifierConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpMoppBvTreeShape(class) => class.serialize(serializer),
            Classes::hkpMoppCode(class) => class.serialize(serializer),
            Classes::hkpMoppCodeCodeInfo(class) => class.serialize(serializer),
            Classes::hkpMoppCodeReindexedTerminal(class) => class.serialize(serializer),
            Classes::hkpMotion(class) => class.serialize(serializer),
            Classes::hkpMotorAction(class) => class.serialize(serializer),
            Classes::hkpMountedBallGun(class) => class.serialize(serializer),
            Classes::hkpMouseSpringAction(class) => class.serialize(serializer),
            Classes::hkpMovingSurfaceModifierConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpMultiRayShape(class) => class.serialize(serializer),
            Classes::hkpMultiRayShapeRay(class) => class.serialize(serializer),
            Classes::hkpMultiSphereShape(class) => class.serialize(serializer),
            Classes::hkpMultithreadedVehicleManager(class) => class.serialize(serializer),
            Classes::hkpNamedMeshMaterial(class) => class.serialize(serializer),
            Classes::hkpNullCollisionFilter(class) => class.serialize(serializer),
            Classes::hkPostFinishAttribute(class) => class.serialize(serializer),
            Classes::hkpOverwritePivotConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpPairCollisionFilter(class) => class.serialize(serializer),
            Classes::hkpPairCollisionFilterMapPairFilterKeyOverrideType(class) => {
                class.serialize(serializer)
            }
            Classes::hkpParametricCurve(class) => class.serialize(serializer),
            Classes::hkpPhantom(class) => class.serialize(serializer),
            Classes::hkpPhantomCallbackShape(class) => class.serialize(serializer),
            Classes::hkpPhysicsData(class) => class.serialize(serializer),
            Classes::hkpPhysicsSystem(class) => class.serialize(serializer),
            Classes::hkpPhysicsSystemWithContacts(class) => class.serialize(serializer),
            Classes::hkpPlaneShape(class) => class.serialize(serializer),
            Classes::hkpPointToPathConstraintData(class) => class.serialize(serializer),
            Classes::hkpPointToPlaneConstraintData(class) => class.serialize(serializer),
            Classes::hkpPointToPlaneConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpPositionConstraintMotor(class) => class.serialize(serializer),
            Classes::hkpPoweredChainData(class) => class.serialize(serializer),
            Classes::hkpPoweredChainDataConstraintInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkpPoweredChainMapper(class) => class.serialize(serializer),
            Classes::hkpPoweredChainMapperLinkInfo(class) => class.serialize(serializer),
            Classes::hkpPoweredChainMapperTarget(class) => class.serialize(serializer),
            Classes::hkpPrismaticConstraintData(class) => class.serialize(serializer),
            Classes::hkpPrismaticConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpProjectileGun(class) => class.serialize(serializer),
            Classes::hkpProperty(class) => class.serialize(serializer),
            Classes::hkpPropertyValue(class) => class.serialize(serializer),
            Classes::hkpPulleyConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpPulleyConstraintData(class) => class.serialize(serializer),
            Classes::hkpPulleyConstraintDataAtoms(class) => class.serialize(serializer),
            Classes::hkpRackAndPinionConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpRackAndPinionConstraintData(class) => class.serialize(serializer),
            Classes::hkpRackAndPinionConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpRagdollConstraintData(class) => class.serialize(serializer),
            Classes::hkpRagdollConstraintDataAtoms(class) => class.serialize(serializer),
            Classes::hkpRagdollLimitsData(class) => class.serialize(serializer),
            Classes::hkpRagdollLimitsDataAtoms(class) => class.serialize(serializer),
            Classes::hkpRagdollMotorConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpRayCollidableFilter(class) => class.serialize(serializer),
            Classes::hkpRayShapeCollectionFilter(class) => class.serialize(serializer),
            Classes::hkpRejectChassisListener(class) => class.serialize(serializer),
            Classes::hkpRemoveTerminalsMoppModifier(class) => class.serialize(serializer),
            Classes::hkpReorientAction(class) => class.serialize(serializer),
            Classes::hkpRigidBody(class) => class.serialize(serializer),
            Classes::hkpRotationalConstraintData(class) => class.serialize(serializer),
            Classes::hkpRotationalConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSampledHeightFieldShape(class) => class.serialize(serializer),
            Classes::hkpSerializedAgentNnEntry(class) => class.serialize(serializer),
            Classes::hkpSerializedDisplayMarker(class) => class.serialize(serializer),
            Classes::hkpSerializedDisplayMarkerList(class) => class.serialize(serializer),
            Classes::hkpSerializedDisplayRbTransforms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSerializedDisplayRbTransformsDisplayTransformPair(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSerializedSubTrack1nInfo(class) => class.serialize(serializer),
            Classes::hkpSerializedTrack1nInfo(class) => class.serialize(serializer),
            Classes::hkpSetLocalRotationsConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSetLocalTransformsConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSetLocalTranslationsConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSetupStabilizationAtom(class) => class.serialize(serializer),
            Classes::hkpShape(class) => class.serialize(serializer),
            Classes::hkpShapeCollection(class) => class.serialize(serializer),
            Classes::hkpShapeCollectionFilter(class) => class.serialize(serializer),
            Classes::hkpShapeContainer(class) => class.serialize(serializer),
            Classes::hkpShapeInfo(class) => class.serialize(serializer),
            Classes::hkpShapeModifier(class) => class.serialize(serializer),
            Classes::hkpShapePhantom(class) => class.serialize(serializer),
            Classes::hkpSimpleContactConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpSimpleContactConstraintDataInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSimpleMeshShape(class) => class.serialize(serializer),
            Classes::hkpSimpleMeshShapeTriangle(class) => class.serialize(serializer),
            Classes::hkpSimpleShapePhantom(class) => class.serialize(serializer),
            Classes::hkpSimpleShapePhantomCollisionDetail(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSimulation(class) => class.serialize(serializer),
            Classes::hkpSingleShapeContainer(class) => class.serialize(serializer),
            Classes::hkpSoftContactModifierConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpSphereMotion(class) => class.serialize(serializer),
            Classes::hkpSphereRepShape(class) => class.serialize(serializer),
            Classes::hkpSphereShape(class) => class.serialize(serializer),
            Classes::hkpSpringAction(class) => class.serialize(serializer),
            Classes::hkpSpringDamperConstraintMotor(class) => class.serialize(serializer),
            Classes::hkpStiffSpringChainData(class) => class.serialize(serializer),
            Classes::hkpStiffSpringChainDataConstraintInfo(class) => {
                class.serialize(serializer)
            }
            Classes::hkpStiffSpringConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpStiffSpringConstraintData(class) => class.serialize(serializer),
            Classes::hkpStiffSpringConstraintDataAtoms(class) => {
                class.serialize(serializer)
            }
            Classes::hkpStorageExtendedMeshShape(class) => class.serialize(serializer),
            Classes::hkpStorageExtendedMeshShapeMaterial(class) => {
                class.serialize(serializer)
            }
            Classes::hkpStorageExtendedMeshShapeMeshSubpartStorage(class) => {
                class.serialize(serializer)
            }
            Classes::hkpStorageExtendedMeshShapeShapeSubpartStorage(class) => {
                class.serialize(serializer)
            }
            Classes::hkpStorageMeshShape(class) => class.serialize(serializer),
            Classes::hkpStorageMeshShapeSubpartStorage(class) => {
                class.serialize(serializer)
            }
            Classes::hkpStorageSampledHeightFieldShape(class) => {
                class.serialize(serializer)
            }
            Classes::hkpThinBoxMotion(class) => class.serialize(serializer),
            Classes::hkpTransformShape(class) => class.serialize(serializer),
            Classes::hkpTriangleShape(class) => class.serialize(serializer),
            Classes::hkpTriggerVolume(class) => class.serialize(serializer),
            Classes::hkpTriggerVolumeEventInfo(class) => class.serialize(serializer),
            Classes::hkpTriSampledHeightFieldBvTreeShape(class) => {
                class.serialize(serializer)
            }
            Classes::hkpTriSampledHeightFieldCollection(class) => {
                class.serialize(serializer)
            }
            Classes::hkpTwistLimitConstraintAtom(class) => class.serialize(serializer),
            Classes::hkpTypedBroadPhaseHandle(class) => class.serialize(serializer),
            Classes::hkpTyremarkPoint(class) => class.serialize(serializer),
            Classes::hkpTyremarksInfo(class) => class.serialize(serializer),
            Classes::hkpTyremarksWheel(class) => class.serialize(serializer),
            Classes::hkpUnaryAction(class) => class.serialize(serializer),
            Classes::hkpVehicleAerodynamics(class) => class.serialize(serializer),
            Classes::hkpVehicleBrake(class) => class.serialize(serializer),
            Classes::hkpVehicleCastBatchingManager(class) => class.serialize(serializer),
            Classes::hkpVehicleData(class) => class.serialize(serializer),
            Classes::hkpVehicleDataWheelComponentParams(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleDefaultAerodynamics(class) => class.serialize(serializer),
            Classes::hkpVehicleDefaultAnalogDriverInput(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleDefaultBrake(class) => class.serialize(serializer),
            Classes::hkpVehicleDefaultBrakeWheelBrakingProperties(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleDefaultEngine(class) => class.serialize(serializer),
            Classes::hkpVehicleDefaultSteering(class) => class.serialize(serializer),
            Classes::hkpVehicleDefaultSuspension(class) => class.serialize(serializer),
            Classes::hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters(
                class,
            ) => class.serialize(serializer),
            Classes::hkpVehicleDefaultTransmission(class) => class.serialize(serializer),
            Classes::hkpVehicleDefaultVelocityDamper(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleDriverInput(class) => class.serialize(serializer),
            Classes::hkpVehicleDriverInputAnalogStatus(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleDriverInputStatus(class) => class.serialize(serializer),
            Classes::hkpVehicleEngine(class) => class.serialize(serializer),
            Classes::hkpVehicleFrictionDescription(class) => class.serialize(serializer),
            Classes::hkpVehicleFrictionDescriptionAxisDescription(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleFrictionStatus(class) => class.serialize(serializer),
            Classes::hkpVehicleFrictionStatusAxisStatus(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleInstance(class) => class.serialize(serializer),
            Classes::hkpVehicleInstanceWheelInfo(class) => class.serialize(serializer),
            Classes::hkpVehicleLinearCastBatchingManager(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleLinearCastWheelCollide(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleLinearCastWheelCollideWheelState(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleManager(class) => class.serialize(serializer),
            Classes::hkpVehicleRayCastBatchingManager(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleRayCastWheelCollide(class) => class.serialize(serializer),
            Classes::hkpVehicleSteering(class) => class.serialize(serializer),
            Classes::hkpVehicleSuspension(class) => class.serialize(serializer),
            Classes::hkpVehicleSuspensionSuspensionWheelParameters(class) => {
                class.serialize(serializer)
            }
            Classes::hkpVehicleTransmission(class) => class.serialize(serializer),
            Classes::hkpVehicleVelocityDamper(class) => class.serialize(serializer),
            Classes::hkpVehicleWheelCollide(class) => class.serialize(serializer),
            Classes::hkpVelocityConstraintMotor(class) => class.serialize(serializer),
            Classes::hkpViscousSurfaceModifierConstraintAtom(class) => {
                class.serialize(serializer)
            }
            Classes::hkpWeldingUtility(class) => class.serialize(serializer),
            Classes::hkpWheelConstraintData(class) => class.serialize(serializer),
            Classes::hkpWheelConstraintDataAtoms(class) => class.serialize(serializer),
            Classes::hkpWorld(class) => class.serialize(serializer),
            Classes::hkpWorldCinfo(class) => class.serialize(serializer),
            Classes::hkpWorldObject(class) => class.serialize(serializer),
            Classes::hkQTransform(class) => class.serialize(serializer),
            Classes::hkRangeInt32Attribute(class) => class.serialize(serializer),
            Classes::hkRangeRealAttribute(class) => class.serialize(serializer),
            Classes::hkReferencedObject(class) => class.serialize(serializer),
            Classes::hkReflectedFileAttribute(class) => class.serialize(serializer),
            Classes::hkResourceBase(class) => class.serialize(serializer),
            Classes::hkResourceContainer(class) => class.serialize(serializer),
            Classes::hkResourceHandle(class) => class.serialize(serializer),
            Classes::hkRootLevelContainer(class) => class.serialize(serializer),
            Classes::hkRootLevelContainerNamedVariant(class) => {
                class.serialize(serializer)
            }
            Classes::hkSemanticsAttribute(class) => class.serialize(serializer),
            Classes::hkSimpleLocalFrame(class) => class.serialize(serializer),
            Classes::hkSphere(class) => class.serialize(serializer),
            Classes::hkSweptTransform(class) => class.serialize(serializer),
            Classes::hkTraceStreamTitle(class) => class.serialize(serializer),
            Classes::hkTrackerSerializableScanSnapshot(class) => {
                class.serialize(serializer)
            }
            Classes::hkTrackerSerializableScanSnapshotAllocation(class) => {
                class.serialize(serializer)
            }
            Classes::hkTrackerSerializableScanSnapshotBlock(class) => {
                class.serialize(serializer)
            }
            Classes::hkUiAttribute(class) => class.serialize(serializer),
            Classes::hkVertexFormat(class) => class.serialize(serializer),
            Classes::hkVertexFormatElement(class) => class.serialize(serializer),
            Classes::hkWorldMemoryAvailableWatchDog(class) => class.serialize(serializer),
            Classes::hkxAnimatedFloat(class) => class.serialize(serializer),
            Classes::hkxAnimatedMatrix(class) => class.serialize(serializer),
            Classes::hkxAnimatedQuaternion(class) => class.serialize(serializer),
            Classes::hkxAnimatedVector(class) => class.serialize(serializer),
            Classes::hkxAttribute(class) => class.serialize(serializer),
            Classes::hkxAttributeGroup(class) => class.serialize(serializer),
            Classes::hkxAttributeHolder(class) => class.serialize(serializer),
            Classes::hkxCamera(class) => class.serialize(serializer),
            Classes::hkxEdgeSelectionChannel(class) => class.serialize(serializer),
            Classes::hkxEnum(class) => class.serialize(serializer),
            Classes::hkxEnumItem(class) => class.serialize(serializer),
            Classes::hkxEnvironment(class) => class.serialize(serializer),
            Classes::hkxEnvironmentVariable(class) => class.serialize(serializer),
            Classes::hkxIndexBuffer(class) => class.serialize(serializer),
            Classes::hkxLight(class) => class.serialize(serializer),
            Classes::hkxMaterial(class) => class.serialize(serializer),
            Classes::hkxMaterialEffect(class) => class.serialize(serializer),
            Classes::hkxMaterialProperty(class) => class.serialize(serializer),
            Classes::hkxMaterialShader(class) => class.serialize(serializer),
            Classes::hkxMaterialShaderSet(class) => class.serialize(serializer),
            Classes::hkxMaterialTextureStage(class) => class.serialize(serializer),
            Classes::hkxMesh(class) => class.serialize(serializer),
            Classes::hkxMeshSection(class) => class.serialize(serializer),
            Classes::hkxMeshUserChannelInfo(class) => class.serialize(serializer),
            Classes::hkxNode(class) => class.serialize(serializer),
            Classes::hkxNodeAnnotationData(class) => class.serialize(serializer),
            Classes::hkxNodeSelectionSet(class) => class.serialize(serializer),
            Classes::hkxScene(class) => class.serialize(serializer),
            Classes::hkxSkinBinding(class) => class.serialize(serializer),
            Classes::hkxSparselyAnimatedBool(class) => class.serialize(serializer),
            Classes::hkxSparselyAnimatedEnum(class) => class.serialize(serializer),
            Classes::hkxSparselyAnimatedInt(class) => class.serialize(serializer),
            Classes::hkxSparselyAnimatedString(class) => class.serialize(serializer),
            Classes::hkxTextureFile(class) => class.serialize(serializer),
            Classes::hkxTextureInplace(class) => class.serialize(serializer),
            Classes::hkxTriangleSelectionChannel(class) => class.serialize(serializer),
            Classes::hkxVertexBuffer(class) => class.serialize(serializer),
            Classes::hkxVertexBufferVertexData(class) => class.serialize(serializer),
            Classes::hkxVertexDescription(class) => class.serialize(serializer),
            Classes::hkxVertexDescriptionElementDecl(class) => {
                class.serialize(serializer)
            }
            Classes::hkxVertexFloatDataChannel(class) => class.serialize(serializer),
            Classes::hkxVertexIntDataChannel(class) => class.serialize(serializer),
            Classes::hkxVertexSelectionChannel(class) => class.serialize(serializer),
            Classes::hkxVertexVectorDataChannel(class) => class.serialize(serializer),
        }
    }
}
/// Deserializes the next class directly into a `Box`.
///
/// # Why a separate, non-inlined function?
/// Without optimization (debug builds) every arm of the big `match` in
/// `visit_class_index` keeps its own stack slot for the unboxed class value, so that
/// frame grows to the sum of all class sizes and overflows the stack
/// (e.g., `thread 'main' has overflowed its stack` with Windows' 1 MiB main stack).
/// Here, each frame holds only one class.
#[inline(never)]
fn next_boxed<'de, A, T>(map: &mut A) -> Result<Box<T>, A::Error>
where
    A: _serde::de::ClassIndexAccess<'de>,
    T: _serde::Deserialize<'de>,
{
    map.next_value().map(Box::new)
}
impl<'a, 'de: 'a> _serde::Deserialize<'de> for Classes<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: _serde::Deserializer<'de>,
    {
        struct ClassesVisitor<'a> {
            marker: core::marker::PhantomData<Classes<'a>>,
        }
        impl<'a, 'de: 'a> _serde::de::Visitor<'de> for ClassesVisitor<'a> {
            type Value = Classes<'a>;
            fn expecting(
                &self,
                formatter: &mut core::fmt::Formatter,
            ) -> core::fmt::Result {
                formatter.write_str("a valid class enum")
            }
            fn visit_class_index<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: _serde::de::ClassIndexAccess<'de>,
            {
                let class_name = map.next_key()?;
                match class_name {
                    "BGSGamebryoSequenceGenerator" => {
                        Ok(Classes::BGSGamebryoSequenceGenerator(next_boxed(&mut map)?))
                    }
                    "BSBoneSwitchGenerator" => {
                        Ok(Classes::BSBoneSwitchGenerator(next_boxed(&mut map)?))
                    }
                    "BSBoneSwitchGeneratorBoneData" => {
                        Ok(Classes::BSBoneSwitchGeneratorBoneData(next_boxed(&mut map)?))
                    }
                    "BSComputeAddBoneAnimModifier" => {
                        Ok(Classes::BSComputeAddBoneAnimModifier(next_boxed(&mut map)?))
                    }
                    "BSCyclicBlendTransitionGenerator" => {
                        Ok(
                            Classes::BSCyclicBlendTransitionGenerator(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "BSDecomposeVectorModifier" => {
                        Ok(Classes::BSDecomposeVectorModifier(next_boxed(&mut map)?))
                    }
                    "BSDirectAtModifier" => {
                        Ok(Classes::BSDirectAtModifier(next_boxed(&mut map)?))
                    }
                    "BSDistTriggerModifier" => {
                        Ok(Classes::BSDistTriggerModifier(next_boxed(&mut map)?))
                    }
                    "BSEventEveryNEventsModifier" => {
                        Ok(Classes::BSEventEveryNEventsModifier(next_boxed(&mut map)?))
                    }
                    "BSEventOnDeactivateModifier" => {
                        Ok(Classes::BSEventOnDeactivateModifier(next_boxed(&mut map)?))
                    }
                    "BSEventOnFalseToTrueModifier" => {
                        Ok(Classes::BSEventOnFalseToTrueModifier(next_boxed(&mut map)?))
                    }
                    "BSGetTimeStepModifier" => {
                        Ok(Classes::BSGetTimeStepModifier(next_boxed(&mut map)?))
                    }
                    "BSInterpValueModifier" => {
                        Ok(Classes::BSInterpValueModifier(next_boxed(&mut map)?))
                    }
                    "BSIsActiveModifier" => {
                        Ok(Classes::BSIsActiveModifier(next_boxed(&mut map)?))
                    }
                    "BSIStateManagerModifier" => {
                        Ok(Classes::BSIStateManagerModifier(next_boxed(&mut map)?))
                    }
                    "BSIStateManagerModifierBSiStateData" => {
                        Ok(
                            Classes::BSIStateManagerModifierBSiStateData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "BSIStateManagerModifierBSIStateManagerStateListener" => {
                        Ok(
                            Classes::BSIStateManagerModifierBSIStateManagerStateListener(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "BSiStateTaggingGenerator" => {
                        Ok(Classes::BSiStateTaggingGenerator(next_boxed(&mut map)?))
                    }
                    "BSLimbIKModifier" => {
                        Ok(Classes::BSLimbIKModifier(next_boxed(&mut map)?))
                    }
                    "BSLookAtModifier" => {
                        Ok(Classes::BSLookAtModifier(next_boxed(&mut map)?))
                    }
                    "BSLookAtModifierBoneData" => {
                        Ok(Classes::BSLookAtModifierBoneData(next_boxed(&mut map)?))
                    }
                    "BSModifyOnceModifier" => {
                        Ok(Classes::BSModifyOnceModifier(next_boxed(&mut map)?))
                    }
                    "BSOffsetAnimationGenerator" => {
                        Ok(Classes::BSOffsetAnimationGenerator(next_boxed(&mut map)?))
                    }
                    "BSPassByTargetTriggerModifier" => {
                        Ok(Classes::BSPassByTargetTriggerModifier(next_boxed(&mut map)?))
                    }
                    "BSRagdollContactListenerModifier" => {
                        Ok(
                            Classes::BSRagdollContactListenerModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "BSSpeedSamplerModifier" => {
                        Ok(Classes::BSSpeedSamplerModifier(next_boxed(&mut map)?))
                    }
                    "BSSynchronizedClipGenerator" => {
                        Ok(Classes::BSSynchronizedClipGenerator(next_boxed(&mut map)?))
                    }
                    "BSTimerModifier" => {
                        Ok(Classes::BSTimerModifier(next_boxed(&mut map)?))
                    }
                    "BSTweenerModifier" => {
                        Ok(Classes::BSTweenerModifier(next_boxed(&mut map)?))
                    }
                    "hkAabb" => Ok(Classes::hkAabb(next_boxed(&mut map)?)),
                    "hkAabbHalf" => Ok(Classes::hkAabbHalf(next_boxed(&mut map)?)),
                    "hkAabbUint32" => Ok(Classes::hkAabbUint32(next_boxed(&mut map)?)),
                    "hkaAnimatedReferenceFrame" => {
                        Ok(Classes::hkaAnimatedReferenceFrame(next_boxed(&mut map)?))
                    }
                    "hkaAnimation" => Ok(Classes::hkaAnimation(next_boxed(&mut map)?)),
                    "hkaAnimationBinding" => {
                        Ok(Classes::hkaAnimationBinding(next_boxed(&mut map)?))
                    }
                    "hkaAnimationContainer" => {
                        Ok(Classes::hkaAnimationContainer(next_boxed(&mut map)?))
                    }
                    "hkaAnimationPreviewColorContainer" => {
                        Ok(
                            Classes::hkaAnimationPreviewColorContainer(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaAnnotationTrack" => {
                        Ok(Classes::hkaAnnotationTrack(next_boxed(&mut map)?))
                    }
                    "hkaAnnotationTrackAnnotation" => {
                        Ok(Classes::hkaAnnotationTrackAnnotation(next_boxed(&mut map)?))
                    }
                    "hkaBone" => Ok(Classes::hkaBone(next_boxed(&mut map)?)),
                    "hkaBoneAttachment" => {
                        Ok(Classes::hkaBoneAttachment(next_boxed(&mut map)?))
                    }
                    "hkaDefaultAnimatedReferenceFrame" => {
                        Ok(
                            Classes::hkaDefaultAnimatedReferenceFrame(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaDeltaCompressedAnimation" => {
                        Ok(Classes::hkaDeltaCompressedAnimation(next_boxed(&mut map)?))
                    }
                    "hkaDeltaCompressedAnimationQuantizationFormat" => {
                        Ok(
                            Classes::hkaDeltaCompressedAnimationQuantizationFormat(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaFootstepAnalysisInfo" => {
                        Ok(Classes::hkaFootstepAnalysisInfo(next_boxed(&mut map)?))
                    }
                    "hkaFootstepAnalysisInfoContainer" => {
                        Ok(
                            Classes::hkaFootstepAnalysisInfoContainer(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaInterleavedUncompressedAnimation" => {
                        Ok(
                            Classes::hkaInterleavedUncompressedAnimation(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaKeyFrameHierarchyUtility" => {
                        Ok(Classes::hkaKeyFrameHierarchyUtility(next_boxed(&mut map)?))
                    }
                    "hkaKeyFrameHierarchyUtilityControlData" => {
                        Ok(
                            Classes::hkaKeyFrameHierarchyUtilityControlData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkAlignSceneToNodeOptions" => {
                        Ok(Classes::hkAlignSceneToNodeOptions(next_boxed(&mut map)?))
                    }
                    "hkaMeshBinding" => {
                        Ok(Classes::hkaMeshBinding(next_boxed(&mut map)?))
                    }
                    "hkaMeshBindingMapping" => {
                        Ok(Classes::hkaMeshBindingMapping(next_boxed(&mut map)?))
                    }
                    "hkaQuantizedAnimation" => {
                        Ok(Classes::hkaQuantizedAnimation(next_boxed(&mut map)?))
                    }
                    "hkaQuantizedAnimationTrackCompressionParams" => {
                        Ok(
                            Classes::hkaQuantizedAnimationTrackCompressionParams(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaRagdollInstance" => {
                        Ok(Classes::hkaRagdollInstance(next_boxed(&mut map)?))
                    }
                    "hkArrayTypeAttribute" => {
                        Ok(Classes::hkArrayTypeAttribute(next_boxed(&mut map)?))
                    }
                    "hkaSkeleton" => Ok(Classes::hkaSkeleton(next_boxed(&mut map)?)),
                    "hkaSkeletonLocalFrameOnBone" => {
                        Ok(Classes::hkaSkeletonLocalFrameOnBone(next_boxed(&mut map)?))
                    }
                    "hkaSkeletonMapper" => {
                        Ok(Classes::hkaSkeletonMapper(next_boxed(&mut map)?))
                    }
                    "hkaSkeletonMapperData" => {
                        Ok(Classes::hkaSkeletonMapperData(next_boxed(&mut map)?))
                    }
                    "hkaSkeletonMapperDataChainMapping" => {
                        Ok(
                            Classes::hkaSkeletonMapperDataChainMapping(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaSkeletonMapperDataSimpleMapping" => {
                        Ok(
                            Classes::hkaSkeletonMapperDataSimpleMapping(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaSplineCompressedAnimation" => {
                        Ok(Classes::hkaSplineCompressedAnimation(next_boxed(&mut map)?))
                    }
                    "hkaSplineCompressedAnimationAnimationCompressionParams" => {
                        Ok(
                            Classes::hkaSplineCompressedAnimationAnimationCompressionParams(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaSplineCompressedAnimationTrackCompressionParams" => {
                        Ok(
                            Classes::hkaSplineCompressedAnimationTrackCompressionParams(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaWaveletCompressedAnimation" => {
                        Ok(Classes::hkaWaveletCompressedAnimation(next_boxed(&mut map)?))
                    }
                    "hkaWaveletCompressedAnimationCompressionParams" => {
                        Ok(
                            Classes::hkaWaveletCompressedAnimationCompressionParams(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkaWaveletCompressedAnimationQuantizationFormat" => {
                        Ok(
                            Classes::hkaWaveletCompressedAnimationQuantizationFormat(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkBaseObject" => Ok(Classes::hkBaseObject(next_boxed(&mut map)?)),
                    "hkbAttachmentModifier" => {
                        Ok(Classes::hkbAttachmentModifier(next_boxed(&mut map)?))
                    }
                    "hkbAttachmentSetup" => {
                        Ok(Classes::hkbAttachmentSetup(next_boxed(&mut map)?))
                    }
                    "hkbAttributeModifier" => {
                        Ok(Classes::hkbAttributeModifier(next_boxed(&mut map)?))
                    }
                    "hkbAttributeModifierAssignment" => {
                        Ok(
                            Classes::hkbAttributeModifierAssignment(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbAuxiliaryNodeInfo" => {
                        Ok(Classes::hkbAuxiliaryNodeInfo(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorEventsInfo" => {
                        Ok(Classes::hkbBehaviorEventsInfo(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorGraph" => {
                        Ok(Classes::hkbBehaviorGraph(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorGraphData" => {
                        Ok(Classes::hkbBehaviorGraphData(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorGraphInternalState" => {
                        Ok(Classes::hkbBehaviorGraphInternalState(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorGraphInternalStateInfo" => {
                        Ok(
                            Classes::hkbBehaviorGraphInternalStateInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbBehaviorGraphStringData" => {
                        Ok(Classes::hkbBehaviorGraphStringData(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorInfo" => {
                        Ok(Classes::hkbBehaviorInfo(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorInfoIdToNamePair" => {
                        Ok(Classes::hkbBehaviorInfoIdToNamePair(next_boxed(&mut map)?))
                    }
                    "hkbBehaviorReferenceGenerator" => {
                        Ok(Classes::hkbBehaviorReferenceGenerator(next_boxed(&mut map)?))
                    }
                    "hkbBindable" => Ok(Classes::hkbBindable(next_boxed(&mut map)?)),
                    "hkbBlendCurveUtils" => {
                        Ok(Classes::hkbBlendCurveUtils(next_boxed(&mut map)?))
                    }
                    "hkbBlenderGenerator" => {
                        Ok(Classes::hkbBlenderGenerator(next_boxed(&mut map)?))
                    }
                    "hkbBlenderGeneratorChild" => {
                        Ok(Classes::hkbBlenderGeneratorChild(next_boxed(&mut map)?))
                    }
                    "hkbBlenderGeneratorChildInternalState" => {
                        Ok(
                            Classes::hkbBlenderGeneratorChildInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbBlenderGeneratorInternalState" => {
                        Ok(
                            Classes::hkbBlenderGeneratorInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbBlendingTransitionEffect" => {
                        Ok(Classes::hkbBlendingTransitionEffect(next_boxed(&mut map)?))
                    }
                    "hkbBlendingTransitionEffectInternalState" => {
                        Ok(
                            Classes::hkbBlendingTransitionEffectInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbBoneIndexArray" => {
                        Ok(Classes::hkbBoneIndexArray(next_boxed(&mut map)?))
                    }
                    "hkbBoneWeightArray" => {
                        Ok(Classes::hkbBoneWeightArray(next_boxed(&mut map)?))
                    }
                    "hkbBoolVariableSequencedData" => {
                        Ok(Classes::hkbBoolVariableSequencedData(next_boxed(&mut map)?))
                    }
                    "hkbBoolVariableSequencedDataSample" => {
                        Ok(
                            Classes::hkbBoolVariableSequencedDataSample(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCameraShakeEventPayload" => {
                        Ok(Classes::hkbCameraShakeEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbCharacter" => Ok(Classes::hkbCharacter(next_boxed(&mut map)?)),
                    "hkbCharacterAddedInfo" => {
                        Ok(Classes::hkbCharacterAddedInfo(next_boxed(&mut map)?))
                    }
                    "hkbCharacterControlCommand" => {
                        Ok(Classes::hkbCharacterControlCommand(next_boxed(&mut map)?))
                    }
                    "hkbCharacterControllerControlData" => {
                        Ok(
                            Classes::hkbCharacterControllerControlData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCharacterControllerModifier" => {
                        Ok(
                            Classes::hkbCharacterControllerModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCharacterControllerModifierInternalState" => {
                        Ok(
                            Classes::hkbCharacterControllerModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCharacterData" => {
                        Ok(Classes::hkbCharacterData(next_boxed(&mut map)?))
                    }
                    "hkbCharacterDataCharacterControllerInfo" => {
                        Ok(
                            Classes::hkbCharacterDataCharacterControllerInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCharacterInfo" => {
                        Ok(Classes::hkbCharacterInfo(next_boxed(&mut map)?))
                    }
                    "hkbCharacterSetup" => {
                        Ok(Classes::hkbCharacterSetup(next_boxed(&mut map)?))
                    }
                    "hkbCharacterSkinInfo" => {
                        Ok(Classes::hkbCharacterSkinInfo(next_boxed(&mut map)?))
                    }
                    "hkbCharacterSteppedInfo" => {
                        Ok(Classes::hkbCharacterSteppedInfo(next_boxed(&mut map)?))
                    }
                    "hkbCharacterStringData" => {
                        Ok(Classes::hkbCharacterStringData(next_boxed(&mut map)?))
                    }
                    "hkbClientCharacterState" => {
                        Ok(Classes::hkbClientCharacterState(next_boxed(&mut map)?))
                    }
                    "hkbClipGenerator" => {
                        Ok(Classes::hkbClipGenerator(next_boxed(&mut map)?))
                    }
                    "hkbClipGeneratorEcho" => {
                        Ok(Classes::hkbClipGeneratorEcho(next_boxed(&mut map)?))
                    }
                    "hkbClipGeneratorInternalState" => {
                        Ok(Classes::hkbClipGeneratorInternalState(next_boxed(&mut map)?))
                    }
                    "hkbClipTrigger" => {
                        Ok(Classes::hkbClipTrigger(next_boxed(&mut map)?))
                    }
                    "hkbClipTriggerArray" => {
                        Ok(Classes::hkbClipTriggerArray(next_boxed(&mut map)?))
                    }
                    "hkbCombineTransformsModifier" => {
                        Ok(Classes::hkbCombineTransformsModifier(next_boxed(&mut map)?))
                    }
                    "hkbCombineTransformsModifierInternalState" => {
                        Ok(
                            Classes::hkbCombineTransformsModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCompiledExpressionSet" => {
                        Ok(Classes::hkbCompiledExpressionSet(next_boxed(&mut map)?))
                    }
                    "hkbCompiledExpressionSetToken" => {
                        Ok(Classes::hkbCompiledExpressionSetToken(next_boxed(&mut map)?))
                    }
                    "hkbComputeDirectionModifier" => {
                        Ok(Classes::hkbComputeDirectionModifier(next_boxed(&mut map)?))
                    }
                    "hkbComputeDirectionModifierInternalState" => {
                        Ok(
                            Classes::hkbComputeDirectionModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbComputeRotationFromAxisAngleModifier" => {
                        Ok(
                            Classes::hkbComputeRotationFromAxisAngleModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbComputeRotationFromAxisAngleModifierInternalState" => {
                        Ok(
                            Classes::hkbComputeRotationFromAxisAngleModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbComputeRotationToTargetModifier" => {
                        Ok(
                            Classes::hkbComputeRotationToTargetModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbComputeRotationToTargetModifierInternalState" => {
                        Ok(
                            Classes::hkbComputeRotationToTargetModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbCondition" => Ok(Classes::hkbCondition(next_boxed(&mut map)?)),
                    "hkbContext" => Ok(Classes::hkbContext(next_boxed(&mut map)?)),
                    "hkbDampingModifier" => {
                        Ok(Classes::hkbDampingModifier(next_boxed(&mut map)?))
                    }
                    "hkbDampingModifierInternalState" => {
                        Ok(
                            Classes::hkbDampingModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbDefaultMessageLog" => {
                        Ok(Classes::hkbDefaultMessageLog(next_boxed(&mut map)?))
                    }
                    "hkbDelayedModifier" => {
                        Ok(Classes::hkbDelayedModifier(next_boxed(&mut map)?))
                    }
                    "hkbDelayedModifierInternalState" => {
                        Ok(
                            Classes::hkbDelayedModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbDetectCloseToGroundModifier" => {
                        Ok(
                            Classes::hkbDetectCloseToGroundModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbDetectCloseToGroundModifierInternalState" => {
                        Ok(
                            Classes::hkbDetectCloseToGroundModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbEvaluateExpressionModifier" => {
                        Ok(Classes::hkbEvaluateExpressionModifier(next_boxed(&mut map)?))
                    }
                    "hkbEvaluateExpressionModifierInternalExpressionData" => {
                        Ok(
                            Classes::hkbEvaluateExpressionModifierInternalExpressionData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbEvaluateExpressionModifierInternalState" => {
                        Ok(
                            Classes::hkbEvaluateExpressionModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbEvaluateHandleModifier" => {
                        Ok(Classes::hkbEvaluateHandleModifier(next_boxed(&mut map)?))
                    }
                    "hkbEvent" => Ok(Classes::hkbEvent(next_boxed(&mut map)?)),
                    "hkbEventBase" => Ok(Classes::hkbEventBase(next_boxed(&mut map)?)),
                    "hkbEventDrivenModifier" => {
                        Ok(Classes::hkbEventDrivenModifier(next_boxed(&mut map)?))
                    }
                    "hkbEventDrivenModifierInternalState" => {
                        Ok(
                            Classes::hkbEventDrivenModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbEventInfo" => Ok(Classes::hkbEventInfo(next_boxed(&mut map)?)),
                    "hkbEventPayload" => {
                        Ok(Classes::hkbEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbEventPayloadList" => {
                        Ok(Classes::hkbEventPayloadList(next_boxed(&mut map)?))
                    }
                    "hkbEventProperty" => {
                        Ok(Classes::hkbEventProperty(next_boxed(&mut map)?))
                    }
                    "hkbEventRaisedInfo" => {
                        Ok(Classes::hkbEventRaisedInfo(next_boxed(&mut map)?))
                    }
                    "hkbEventRangeData" => {
                        Ok(Classes::hkbEventRangeData(next_boxed(&mut map)?))
                    }
                    "hkbEventRangeDataArray" => {
                        Ok(Classes::hkbEventRangeDataArray(next_boxed(&mut map)?))
                    }
                    "hkbEventSequencedData" => {
                        Ok(Classes::hkbEventSequencedData(next_boxed(&mut map)?))
                    }
                    "hkbEventSequencedDataSequencedEvent" => {
                        Ok(
                            Classes::hkbEventSequencedDataSequencedEvent(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbEventsFromRangeModifier" => {
                        Ok(Classes::hkbEventsFromRangeModifier(next_boxed(&mut map)?))
                    }
                    "hkbEventsFromRangeModifierInternalState" => {
                        Ok(
                            Classes::hkbEventsFromRangeModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbExpressionCondition" => {
                        Ok(Classes::hkbExpressionCondition(next_boxed(&mut map)?))
                    }
                    "hkbExpressionData" => {
                        Ok(Classes::hkbExpressionData(next_boxed(&mut map)?))
                    }
                    "hkbExpressionDataArray" => {
                        Ok(Classes::hkbExpressionDataArray(next_boxed(&mut map)?))
                    }
                    "hkbExtractRagdollPoseModifier" => {
                        Ok(Classes::hkbExtractRagdollPoseModifier(next_boxed(&mut map)?))
                    }
                    "hkbFootIkControlData" => {
                        Ok(Classes::hkbFootIkControlData(next_boxed(&mut map)?))
                    }
                    "hkbFootIkControlsModifier" => {
                        Ok(Classes::hkbFootIkControlsModifier(next_boxed(&mut map)?))
                    }
                    "hkbFootIkControlsModifierLeg" => {
                        Ok(Classes::hkbFootIkControlsModifierLeg(next_boxed(&mut map)?))
                    }
                    "hkbFootIkDriverInfo" => {
                        Ok(Classes::hkbFootIkDriverInfo(next_boxed(&mut map)?))
                    }
                    "hkbFootIkDriverInfoLeg" => {
                        Ok(Classes::hkbFootIkDriverInfoLeg(next_boxed(&mut map)?))
                    }
                    "hkbFootIkGains" => {
                        Ok(Classes::hkbFootIkGains(next_boxed(&mut map)?))
                    }
                    "hkbFootIkModifier" => {
                        Ok(Classes::hkbFootIkModifier(next_boxed(&mut map)?))
                    }
                    "hkbFootIkModifierInternalLegData" => {
                        Ok(
                            Classes::hkbFootIkModifierInternalLegData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbFootIkModifierLeg" => {
                        Ok(Classes::hkbFootIkModifierLeg(next_boxed(&mut map)?))
                    }
                    "hkbGenerator" => Ok(Classes::hkbGenerator(next_boxed(&mut map)?)),
                    "hkbGeneratorOutputListener" => {
                        Ok(Classes::hkbGeneratorOutputListener(next_boxed(&mut map)?))
                    }
                    "hkbGeneratorSyncInfo" => {
                        Ok(Classes::hkbGeneratorSyncInfo(next_boxed(&mut map)?))
                    }
                    "hkbGeneratorSyncInfoSyncPoint" => {
                        Ok(Classes::hkbGeneratorSyncInfoSyncPoint(next_boxed(&mut map)?))
                    }
                    "hkbGeneratorTransitionEffect" => {
                        Ok(Classes::hkbGeneratorTransitionEffect(next_boxed(&mut map)?))
                    }
                    "hkbGeneratorTransitionEffectInternalState" => {
                        Ok(
                            Classes::hkbGeneratorTransitionEffectInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbGetHandleOnBoneModifier" => {
                        Ok(Classes::hkbGetHandleOnBoneModifier(next_boxed(&mut map)?))
                    }
                    "hkbGetUpModifier" => {
                        Ok(Classes::hkbGetUpModifier(next_boxed(&mut map)?))
                    }
                    "hkbGetUpModifierInternalState" => {
                        Ok(Classes::hkbGetUpModifierInternalState(next_boxed(&mut map)?))
                    }
                    "hkbGetWorldFromModelModifier" => {
                        Ok(Classes::hkbGetWorldFromModelModifier(next_boxed(&mut map)?))
                    }
                    "hkbGetWorldFromModelModifierInternalState" => {
                        Ok(
                            Classes::hkbGetWorldFromModelModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbHandIkControlData" => {
                        Ok(Classes::hkbHandIkControlData(next_boxed(&mut map)?))
                    }
                    "hkbHandIkControlsModifier" => {
                        Ok(Classes::hkbHandIkControlsModifier(next_boxed(&mut map)?))
                    }
                    "hkbHandIkControlsModifierHand" => {
                        Ok(Classes::hkbHandIkControlsModifierHand(next_boxed(&mut map)?))
                    }
                    "hkbHandIkDriverInfo" => {
                        Ok(Classes::hkbHandIkDriverInfo(next_boxed(&mut map)?))
                    }
                    "hkbHandIkDriverInfoHand" => {
                        Ok(Classes::hkbHandIkDriverInfoHand(next_boxed(&mut map)?))
                    }
                    "hkbHandIkModifier" => {
                        Ok(Classes::hkbHandIkModifier(next_boxed(&mut map)?))
                    }
                    "hkbHandIkModifierHand" => {
                        Ok(Classes::hkbHandIkModifierHand(next_boxed(&mut map)?))
                    }
                    "hkbHandle" => Ok(Classes::hkbHandle(next_boxed(&mut map)?)),
                    "hkbIntEventPayload" => {
                        Ok(Classes::hkbIntEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbIntVariableSequencedData" => {
                        Ok(Classes::hkbIntVariableSequencedData(next_boxed(&mut map)?))
                    }
                    "hkbIntVariableSequencedDataSample" => {
                        Ok(
                            Classes::hkbIntVariableSequencedDataSample(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkBitField" => Ok(Classes::hkBitField(next_boxed(&mut map)?)),
                    "hkbKeyframeBonesModifier" => {
                        Ok(Classes::hkbKeyframeBonesModifier(next_boxed(&mut map)?))
                    }
                    "hkbKeyframeBonesModifierKeyframeInfo" => {
                        Ok(
                            Classes::hkbKeyframeBonesModifierKeyframeInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbLinkedSymbolInfo" => {
                        Ok(Classes::hkbLinkedSymbolInfo(next_boxed(&mut map)?))
                    }
                    "hkbLookAtModifier" => {
                        Ok(Classes::hkbLookAtModifier(next_boxed(&mut map)?))
                    }
                    "hkbLookAtModifierInternalState" => {
                        Ok(
                            Classes::hkbLookAtModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbManualSelectorGenerator" => {
                        Ok(Classes::hkbManualSelectorGenerator(next_boxed(&mut map)?))
                    }
                    "hkbManualSelectorGeneratorInternalState" => {
                        Ok(
                            Classes::hkbManualSelectorGeneratorInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbMessageLog" => Ok(Classes::hkbMessageLog(next_boxed(&mut map)?)),
                    "hkbMirroredSkeletonInfo" => {
                        Ok(Classes::hkbMirroredSkeletonInfo(next_boxed(&mut map)?))
                    }
                    "hkbMirrorModifier" => {
                        Ok(Classes::hkbMirrorModifier(next_boxed(&mut map)?))
                    }
                    "hkbModifier" => Ok(Classes::hkbModifier(next_boxed(&mut map)?)),
                    "hkbModifierGenerator" => {
                        Ok(Classes::hkbModifierGenerator(next_boxed(&mut map)?))
                    }
                    "hkbModifierList" => {
                        Ok(Classes::hkbModifierList(next_boxed(&mut map)?))
                    }
                    "hkbModifierWrapper" => {
                        Ok(Classes::hkbModifierWrapper(next_boxed(&mut map)?))
                    }
                    "hkbMoveCharacterModifier" => {
                        Ok(Classes::hkbMoveCharacterModifier(next_boxed(&mut map)?))
                    }
                    "hkbMoveCharacterModifierInternalState" => {
                        Ok(
                            Classes::hkbMoveCharacterModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbNamedEventPayload" => {
                        Ok(Classes::hkbNamedEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbNamedIntEventPayload" => {
                        Ok(Classes::hkbNamedIntEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbNamedRealEventPayload" => {
                        Ok(Classes::hkbNamedRealEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbNamedStringEventPayload" => {
                        Ok(Classes::hkbNamedStringEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbNode" => Ok(Classes::hkbNode(next_boxed(&mut map)?)),
                    "hkbNodeInternalStateInfo" => {
                        Ok(Classes::hkbNodeInternalStateInfo(next_boxed(&mut map)?))
                    }
                    "hkbParticleSystemEventPayload" => {
                        Ok(Classes::hkbParticleSystemEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbPoseMatchingGenerator" => {
                        Ok(Classes::hkbPoseMatchingGenerator(next_boxed(&mut map)?))
                    }
                    "hkbPoseMatchingGeneratorInternalState" => {
                        Ok(
                            Classes::hkbPoseMatchingGeneratorInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbPoweredRagdollControlData" => {
                        Ok(Classes::hkbPoweredRagdollControlData(next_boxed(&mut map)?))
                    }
                    "hkbPoweredRagdollControlsModifier" => {
                        Ok(
                            Classes::hkbPoweredRagdollControlsModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbProjectData" => {
                        Ok(Classes::hkbProjectData(next_boxed(&mut map)?))
                    }
                    "hkbProjectStringData" => {
                        Ok(Classes::hkbProjectStringData(next_boxed(&mut map)?))
                    }
                    "hkbProxyModifier" => {
                        Ok(Classes::hkbProxyModifier(next_boxed(&mut map)?))
                    }
                    "hkbProxyModifierProxyInfo" => {
                        Ok(Classes::hkbProxyModifierProxyInfo(next_boxed(&mut map)?))
                    }
                    "hkbRaiseEventCommand" => {
                        Ok(Classes::hkbRaiseEventCommand(next_boxed(&mut map)?))
                    }
                    "hkbRealEventPayload" => {
                        Ok(Classes::hkbRealEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbRealVariableSequencedData" => {
                        Ok(Classes::hkbRealVariableSequencedData(next_boxed(&mut map)?))
                    }
                    "hkbRealVariableSequencedDataSample" => {
                        Ok(
                            Classes::hkbRealVariableSequencedDataSample(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbReferencePoseGenerator" => {
                        Ok(Classes::hkbReferencePoseGenerator(next_boxed(&mut map)?))
                    }
                    "hkbRegisteredGenerator" => {
                        Ok(Classes::hkbRegisteredGenerator(next_boxed(&mut map)?))
                    }
                    "hkbRigidBodyRagdollControlData" => {
                        Ok(
                            Classes::hkbRigidBodyRagdollControlData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbRigidBodyRagdollControlsModifier" => {
                        Ok(
                            Classes::hkbRigidBodyRagdollControlsModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbRoleAttribute" => {
                        Ok(Classes::hkbRoleAttribute(next_boxed(&mut map)?))
                    }
                    "hkbRotateCharacterModifier" => {
                        Ok(Classes::hkbRotateCharacterModifier(next_boxed(&mut map)?))
                    }
                    "hkbRotateCharacterModifierInternalState" => {
                        Ok(
                            Classes::hkbRotateCharacterModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbSenseHandleModifier" => {
                        Ok(Classes::hkbSenseHandleModifier(next_boxed(&mut map)?))
                    }
                    "hkbSenseHandleModifierRange" => {
                        Ok(Classes::hkbSenseHandleModifierRange(next_boxed(&mut map)?))
                    }
                    "hkbSequence" => Ok(Classes::hkbSequence(next_boxed(&mut map)?)),
                    "hkbSequencedData" => {
                        Ok(Classes::hkbSequencedData(next_boxed(&mut map)?))
                    }
                    "hkbSequenceInternalState" => {
                        Ok(Classes::hkbSequenceInternalState(next_boxed(&mut map)?))
                    }
                    "hkbSequenceStringData" => {
                        Ok(Classes::hkbSequenceStringData(next_boxed(&mut map)?))
                    }
                    "hkbSetBehaviorCommand" => {
                        Ok(Classes::hkbSetBehaviorCommand(next_boxed(&mut map)?))
                    }
                    "hkbSetLocalTimeOfClipGeneratorCommand" => {
                        Ok(
                            Classes::hkbSetLocalTimeOfClipGeneratorCommand(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbSetNodePropertyCommand" => {
                        Ok(Classes::hkbSetNodePropertyCommand(next_boxed(&mut map)?))
                    }
                    "hkbSetWordVariableCommand" => {
                        Ok(Classes::hkbSetWordVariableCommand(next_boxed(&mut map)?))
                    }
                    "hkbSetWorldFromModelModifier" => {
                        Ok(Classes::hkbSetWorldFromModelModifier(next_boxed(&mut map)?))
                    }
                    "hkbSimulationControlCommand" => {
                        Ok(Classes::hkbSimulationControlCommand(next_boxed(&mut map)?))
                    }
                    "hkbSimulationStateInfo" => {
                        Ok(Classes::hkbSimulationStateInfo(next_boxed(&mut map)?))
                    }
                    "hkbStateChooser" => {
                        Ok(Classes::hkbStateChooser(next_boxed(&mut map)?))
                    }
                    "hkbStateListener" => {
                        Ok(Classes::hkbStateListener(next_boxed(&mut map)?))
                    }
                    "hkbStateMachine" => {
                        Ok(Classes::hkbStateMachine(next_boxed(&mut map)?))
                    }
                    "hkbStateMachineActiveTransitionInfo" => {
                        Ok(
                            Classes::hkbStateMachineActiveTransitionInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStateMachineDelayedTransitionInfo" => {
                        Ok(
                            Classes::hkbStateMachineDelayedTransitionInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStateMachineEventPropertyArray" => {
                        Ok(
                            Classes::hkbStateMachineEventPropertyArray(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStateMachineInternalState" => {
                        Ok(Classes::hkbStateMachineInternalState(next_boxed(&mut map)?))
                    }
                    "hkbStateMachineNestedStateMachineData" => {
                        Ok(
                            Classes::hkbStateMachineNestedStateMachineData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStateMachineProspectiveTransitionInfo" => {
                        Ok(
                            Classes::hkbStateMachineProspectiveTransitionInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStateMachineStateInfo" => {
                        Ok(Classes::hkbStateMachineStateInfo(next_boxed(&mut map)?))
                    }
                    "hkbStateMachineTimeInterval" => {
                        Ok(Classes::hkbStateMachineTimeInterval(next_boxed(&mut map)?))
                    }
                    "hkbStateMachineTransitionInfo" => {
                        Ok(Classes::hkbStateMachineTransitionInfo(next_boxed(&mut map)?))
                    }
                    "hkbStateMachineTransitionInfoArray" => {
                        Ok(
                            Classes::hkbStateMachineTransitionInfoArray(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStateMachineTransitionInfoReference" => {
                        Ok(
                            Classes::hkbStateMachineTransitionInfoReference(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbStringCondition" => {
                        Ok(Classes::hkbStringCondition(next_boxed(&mut map)?))
                    }
                    "hkbStringEventPayload" => {
                        Ok(Classes::hkbStringEventPayload(next_boxed(&mut map)?))
                    }
                    "hkbTestStateChooser" => {
                        Ok(Classes::hkbTestStateChooser(next_boxed(&mut map)?))
                    }
                    "hkbTimerModifier" => {
                        Ok(Classes::hkbTimerModifier(next_boxed(&mut map)?))
                    }
                    "hkbTimerModifierInternalState" => {
                        Ok(Classes::hkbTimerModifierInternalState(next_boxed(&mut map)?))
                    }
                    "hkbTransformVectorModifier" => {
                        Ok(Classes::hkbTransformVectorModifier(next_boxed(&mut map)?))
                    }
                    "hkbTransformVectorModifierInternalState" => {
                        Ok(
                            Classes::hkbTransformVectorModifierInternalState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkbTransitionEffect" => {
                        Ok(Classes::hkbTransitionEffect(next_boxed(&mut map)?))
                    }
                    "hkbTwistModifier" => {
                        Ok(Classes::hkbTwistModifier(next_boxed(&mut map)?))
                    }
                    "hkbVariableBindingSet" => {
                        Ok(Classes::hkbVariableBindingSet(next_boxed(&mut map)?))
                    }
                    "hkbVariableBindingSetBinding" => {
                        Ok(Classes::hkbVariableBindingSetBinding(next_boxed(&mut map)?))
                    }
                    "hkbVariableInfo" => {
                        Ok(Classes::hkbVariableInfo(next_boxed(&mut map)?))
                    }
                    "hkbVariableValue" => {
                        Ok(Classes::hkbVariableValue(next_boxed(&mut map)?))
                    }
                    "hkbVariableValueSet" => {
                        Ok(Classes::hkbVariableValueSet(next_boxed(&mut map)?))
                    }
                    "hkbWorldEnums" => Ok(Classes::hkbWorldEnums(next_boxed(&mut map)?)),
                    "hkbWorldFromModelModeData" => {
                        Ok(Classes::hkbWorldFromModelModeData(next_boxed(&mut map)?))
                    }
                    "hkClass" => Ok(Classes::hkClass(next_boxed(&mut map)?)),
                    "hkClassEnum" => Ok(Classes::hkClassEnum(next_boxed(&mut map)?)),
                    "hkClassEnumItem" => {
                        Ok(Classes::hkClassEnumItem(next_boxed(&mut map)?))
                    }
                    "hkClassMember" => Ok(Classes::hkClassMember(next_boxed(&mut map)?)),
                    "hkColor" => Ok(Classes::hkColor(next_boxed(&mut map)?)),
                    "hkContactPoint" => {
                        Ok(Classes::hkContactPoint(next_boxed(&mut map)?))
                    }
                    "hkContactPointMaterial" => {
                        Ok(Classes::hkContactPointMaterial(next_boxed(&mut map)?))
                    }
                    "hkCustomAttributes" => {
                        Ok(Classes::hkCustomAttributes(next_boxed(&mut map)?))
                    }
                    "hkCustomAttributesAttribute" => {
                        Ok(Classes::hkCustomAttributesAttribute(next_boxed(&mut map)?))
                    }
                    "hkDataObjectTypeAttribute" => {
                        Ok(Classes::hkDataObjectTypeAttribute(next_boxed(&mut map)?))
                    }
                    "hkDescriptionAttribute" => {
                        Ok(Classes::hkDescriptionAttribute(next_boxed(&mut map)?))
                    }
                    "hkDocumentationAttribute" => {
                        Ok(Classes::hkDocumentationAttribute(next_boxed(&mut map)?))
                    }
                    "hkGeometry" => Ok(Classes::hkGeometry(next_boxed(&mut map)?)),
                    "hkGeometryTriangle" => {
                        Ok(Classes::hkGeometryTriangle(next_boxed(&mut map)?))
                    }
                    "hkGizmoAttribute" => {
                        Ok(Classes::hkGizmoAttribute(next_boxed(&mut map)?))
                    }
                    "hkHalf8" => Ok(Classes::hkHalf8(next_boxed(&mut map)?)),
                    "hkIndexedTransformSet" => {
                        Ok(Classes::hkIndexedTransformSet(next_boxed(&mut map)?))
                    }
                    "hkLinkAttribute" => {
                        Ok(Classes::hkLinkAttribute(next_boxed(&mut map)?))
                    }
                    "hkLocalFrame" => Ok(Classes::hkLocalFrame(next_boxed(&mut map)?)),
                    "hkLocalFrameGroup" => {
                        Ok(Classes::hkLocalFrameGroup(next_boxed(&mut map)?))
                    }
                    "hkMemoryMeshBody" => {
                        Ok(Classes::hkMemoryMeshBody(next_boxed(&mut map)?))
                    }
                    "hkMemoryMeshMaterial" => {
                        Ok(Classes::hkMemoryMeshMaterial(next_boxed(&mut map)?))
                    }
                    "hkMemoryMeshShape" => {
                        Ok(Classes::hkMemoryMeshShape(next_boxed(&mut map)?))
                    }
                    "hkMemoryMeshTexture" => {
                        Ok(Classes::hkMemoryMeshTexture(next_boxed(&mut map)?))
                    }
                    "hkMemoryMeshVertexBuffer" => {
                        Ok(Classes::hkMemoryMeshVertexBuffer(next_boxed(&mut map)?))
                    }
                    "hkMemoryResourceContainer" => {
                        Ok(Classes::hkMemoryResourceContainer(next_boxed(&mut map)?))
                    }
                    "hkMemoryResourceHandle" => {
                        Ok(Classes::hkMemoryResourceHandle(next_boxed(&mut map)?))
                    }
                    "hkMemoryResourceHandleExternalLink" => {
                        Ok(
                            Classes::hkMemoryResourceHandleExternalLink(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkMemoryTrackerAttribute" => {
                        Ok(Classes::hkMemoryTrackerAttribute(next_boxed(&mut map)?))
                    }
                    "hkMeshBody" => Ok(Classes::hkMeshBody(next_boxed(&mut map)?)),
                    "hkMeshBoneIndexMapping" => {
                        Ok(Classes::hkMeshBoneIndexMapping(next_boxed(&mut map)?))
                    }
                    "hkMeshMaterial" => {
                        Ok(Classes::hkMeshMaterial(next_boxed(&mut map)?))
                    }
                    "hkMeshSection" => Ok(Classes::hkMeshSection(next_boxed(&mut map)?)),
                    "hkMeshSectionCinfo" => {
                        Ok(Classes::hkMeshSectionCinfo(next_boxed(&mut map)?))
                    }
                    "hkMeshShape" => Ok(Classes::hkMeshShape(next_boxed(&mut map)?)),
                    "hkMeshTexture" => Ok(Classes::hkMeshTexture(next_boxed(&mut map)?)),
                    "hkMeshVertexBuffer" => {
                        Ok(Classes::hkMeshVertexBuffer(next_boxed(&mut map)?))
                    }
                    "hkModelerNodeTypeAttribute" => {
                        Ok(Classes::hkModelerNodeTypeAttribute(next_boxed(&mut map)?))
                    }
                    "hkMonitorStreamColorTable" => {
                        Ok(Classes::hkMonitorStreamColorTable(next_boxed(&mut map)?))
                    }
                    "hkMonitorStreamColorTableColorPair" => {
                        Ok(
                            Classes::hkMonitorStreamColorTableColorPair(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkMonitorStreamFrameInfo" => {
                        Ok(Classes::hkMonitorStreamFrameInfo(next_boxed(&mut map)?))
                    }
                    "hkMonitorStreamStringMap" => {
                        Ok(Classes::hkMonitorStreamStringMap(next_boxed(&mut map)?))
                    }
                    "hkMonitorStreamStringMapStringMap" => {
                        Ok(
                            Classes::hkMonitorStreamStringMapStringMap(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkMoppBvTreeShapeBase" => {
                        Ok(Classes::hkMoppBvTreeShapeBase(next_boxed(&mut map)?))
                    }
                    "hkMotionState" => Ok(Classes::hkMotionState(next_boxed(&mut map)?)),
                    "hkMultipleVertexBuffer" => {
                        Ok(Classes::hkMultipleVertexBuffer(next_boxed(&mut map)?))
                    }
                    "hkMultipleVertexBufferElementInfo" => {
                        Ok(
                            Classes::hkMultipleVertexBufferElementInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkMultipleVertexBufferLockedElement" => {
                        Ok(
                            Classes::hkMultipleVertexBufferLockedElement(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkMultipleVertexBufferVertexBufferInfo" => {
                        Ok(
                            Classes::hkMultipleVertexBufferVertexBufferInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkMultiThreadCheck" => {
                        Ok(Classes::hkMultiThreadCheck(next_boxed(&mut map)?))
                    }
                    "hkp2dAngConstraintAtom" => {
                        Ok(Classes::hkp2dAngConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpAabbPhantom" => {
                        Ok(Classes::hkpAabbPhantom(next_boxed(&mut map)?))
                    }
                    "hkPackedVector3" => {
                        Ok(Classes::hkPackedVector3(next_boxed(&mut map)?))
                    }
                    "hkPackfileHeader" => {
                        Ok(Classes::hkPackfileHeader(next_boxed(&mut map)?))
                    }
                    "hkPackfileSectionHeader" => {
                        Ok(Classes::hkPackfileSectionHeader(next_boxed(&mut map)?))
                    }
                    "hkpAction" => Ok(Classes::hkpAction(next_boxed(&mut map)?)),
                    "hkpAgent1nSector" => {
                        Ok(Classes::hkpAgent1nSector(next_boxed(&mut map)?))
                    }
                    "hkpAngConstraintAtom" => {
                        Ok(Classes::hkpAngConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpAngFrictionConstraintAtom" => {
                        Ok(Classes::hkpAngFrictionConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpAngLimitConstraintAtom" => {
                        Ok(Classes::hkpAngLimitConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpAngMotorConstraintAtom" => {
                        Ok(Classes::hkpAngMotorConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpAngularDashpotAction" => {
                        Ok(Classes::hkpAngularDashpotAction(next_boxed(&mut map)?))
                    }
                    "hkpArrayAction" => {
                        Ok(Classes::hkpArrayAction(next_boxed(&mut map)?))
                    }
                    "hkpBallAndSocketConstraintData" => {
                        Ok(
                            Classes::hkpBallAndSocketConstraintData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpBallAndSocketConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpBallAndSocketConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpBallGun" => Ok(Classes::hkpBallGun(next_boxed(&mut map)?)),
                    "hkpBallSocketChainData" => {
                        Ok(Classes::hkpBallSocketChainData(next_boxed(&mut map)?))
                    }
                    "hkpBallSocketChainDataConstraintInfo" => {
                        Ok(
                            Classes::hkpBallSocketChainDataConstraintInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpBallSocketConstraintAtom" => {
                        Ok(Classes::hkpBallSocketConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpBinaryAction" => {
                        Ok(Classes::hkpBinaryAction(next_boxed(&mut map)?))
                    }
                    "hkpBoxMotion" => Ok(Classes::hkpBoxMotion(next_boxed(&mut map)?)),
                    "hkpBoxShape" => Ok(Classes::hkpBoxShape(next_boxed(&mut map)?)),
                    "hkpBreakableBody" => {
                        Ok(Classes::hkpBreakableBody(next_boxed(&mut map)?))
                    }
                    "hkpBreakableConstraintData" => {
                        Ok(Classes::hkpBreakableConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpBridgeAtoms" => {
                        Ok(Classes::hkpBridgeAtoms(next_boxed(&mut map)?))
                    }
                    "hkpBridgeConstraintAtom" => {
                        Ok(Classes::hkpBridgeConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpBroadPhaseHandle" => {
                        Ok(Classes::hkpBroadPhaseHandle(next_boxed(&mut map)?))
                    }
                    "hkpBvShape" => Ok(Classes::hkpBvShape(next_boxed(&mut map)?)),
                    "hkpBvTreeShape" => {
                        Ok(Classes::hkpBvTreeShape(next_boxed(&mut map)?))
                    }
                    "hkpCachingShapePhantom" => {
                        Ok(Classes::hkpCachingShapePhantom(next_boxed(&mut map)?))
                    }
                    "hkpCallbackConstraintMotor" => {
                        Ok(Classes::hkpCallbackConstraintMotor(next_boxed(&mut map)?))
                    }
                    "hkpCapsuleShape" => {
                        Ok(Classes::hkpCapsuleShape(next_boxed(&mut map)?))
                    }
                    "hkpCdBody" => Ok(Classes::hkpCdBody(next_boxed(&mut map)?)),
                    "hkpCenterOfMassChangerModifierConstraintAtom" => {
                        Ok(
                            Classes::hkpCenterOfMassChangerModifierConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpCharacterControllerCinfo" => {
                        Ok(Classes::hkpCharacterControllerCinfo(next_boxed(&mut map)?))
                    }
                    "hkpCharacterMotion" => {
                        Ok(Classes::hkpCharacterMotion(next_boxed(&mut map)?))
                    }
                    "hkpCharacterProxyCinfo" => {
                        Ok(Classes::hkpCharacterProxyCinfo(next_boxed(&mut map)?))
                    }
                    "hkpCharacterRigidBodyCinfo" => {
                        Ok(Classes::hkpCharacterRigidBodyCinfo(next_boxed(&mut map)?))
                    }
                    "hkpCogWheelConstraintAtom" => {
                        Ok(Classes::hkpCogWheelConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpCogWheelConstraintData" => {
                        Ok(Classes::hkpCogWheelConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpCogWheelConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpCogWheelConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpCollidable" => Ok(Classes::hkpCollidable(next_boxed(&mut map)?)),
                    "hkpCollidableBoundingVolumeData" => {
                        Ok(
                            Classes::hkpCollidableBoundingVolumeData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpCollidableCollidableFilter" => {
                        Ok(Classes::hkpCollidableCollidableFilter(next_boxed(&mut map)?))
                    }
                    "hkpCollisionFilter" => {
                        Ok(Classes::hkpCollisionFilter(next_boxed(&mut map)?))
                    }
                    "hkpCollisionFilterList" => {
                        Ok(Classes::hkpCollisionFilterList(next_boxed(&mut map)?))
                    }
                    "hkpCompressedMeshShape" => {
                        Ok(Classes::hkpCompressedMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpCompressedMeshShapeBigTriangle" => {
                        Ok(
                            Classes::hkpCompressedMeshShapeBigTriangle(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpCompressedMeshShapeChunk" => {
                        Ok(Classes::hkpCompressedMeshShapeChunk(next_boxed(&mut map)?))
                    }
                    "hkpCompressedMeshShapeConvexPiece" => {
                        Ok(
                            Classes::hkpCompressedMeshShapeConvexPiece(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpCompressedSampledHeightFieldShape" => {
                        Ok(
                            Classes::hkpCompressedSampledHeightFieldShape(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpConeLimitConstraintAtom" => {
                        Ok(Classes::hkpConeLimitConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpConstrainedSystemFilter" => {
                        Ok(Classes::hkpConstrainedSystemFilter(next_boxed(&mut map)?))
                    }
                    "hkpConstraintAtom" => {
                        Ok(Classes::hkpConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpConstraintChainData" => {
                        Ok(Classes::hkpConstraintChainData(next_boxed(&mut map)?))
                    }
                    "hkpConstraintChainInstance" => {
                        Ok(Classes::hkpConstraintChainInstance(next_boxed(&mut map)?))
                    }
                    "hkpConstraintChainInstanceAction" => {
                        Ok(
                            Classes::hkpConstraintChainInstanceAction(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpConstraintCollisionFilter" => {
                        Ok(Classes::hkpConstraintCollisionFilter(next_boxed(&mut map)?))
                    }
                    "hkpConstraintData" => {
                        Ok(Classes::hkpConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpConstraintInstance" => {
                        Ok(Classes::hkpConstraintInstance(next_boxed(&mut map)?))
                    }
                    "hkpConstraintInstanceSmallArraySerializeOverrideType" => {
                        Ok(
                            Classes::hkpConstraintInstanceSmallArraySerializeOverrideType(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpConstraintMotor" => {
                        Ok(Classes::hkpConstraintMotor(next_boxed(&mut map)?))
                    }
                    "hkpConvexListFilter" => {
                        Ok(Classes::hkpConvexListFilter(next_boxed(&mut map)?))
                    }
                    "hkpConvexListShape" => {
                        Ok(Classes::hkpConvexListShape(next_boxed(&mut map)?))
                    }
                    "hkpConvexPieceMeshShape" => {
                        Ok(Classes::hkpConvexPieceMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpConvexPieceStreamData" => {
                        Ok(Classes::hkpConvexPieceStreamData(next_boxed(&mut map)?))
                    }
                    "hkpConvexShape" => {
                        Ok(Classes::hkpConvexShape(next_boxed(&mut map)?))
                    }
                    "hkpConvexTransformShape" => {
                        Ok(Classes::hkpConvexTransformShape(next_boxed(&mut map)?))
                    }
                    "hkpConvexTransformShapeBase" => {
                        Ok(Classes::hkpConvexTransformShapeBase(next_boxed(&mut map)?))
                    }
                    "hkpConvexTranslateShape" => {
                        Ok(Classes::hkpConvexTranslateShape(next_boxed(&mut map)?))
                    }
                    "hkpConvexVerticesConnectivity" => {
                        Ok(Classes::hkpConvexVerticesConnectivity(next_boxed(&mut map)?))
                    }
                    "hkpConvexVerticesShape" => {
                        Ok(Classes::hkpConvexVerticesShape(next_boxed(&mut map)?))
                    }
                    "hkpConvexVerticesShapeFourVectors" => {
                        Ok(
                            Classes::hkpConvexVerticesShapeFourVectors(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpCylinderShape" => {
                        Ok(Classes::hkpCylinderShape(next_boxed(&mut map)?))
                    }
                    "hkpDashpotAction" => {
                        Ok(Classes::hkpDashpotAction(next_boxed(&mut map)?))
                    }
                    "hkpDefaultConvexListFilter" => {
                        Ok(Classes::hkpDefaultConvexListFilter(next_boxed(&mut map)?))
                    }
                    "hkpDefaultWorldMemoryWatchDog" => {
                        Ok(Classes::hkpDefaultWorldMemoryWatchDog(next_boxed(&mut map)?))
                    }
                    "hkpDisableEntityCollisionFilter" => {
                        Ok(
                            Classes::hkpDisableEntityCollisionFilter(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpDisplayBindingData" => {
                        Ok(Classes::hkpDisplayBindingData(next_boxed(&mut map)?))
                    }
                    "hkpDisplayBindingDataPhysicsSystem" => {
                        Ok(
                            Classes::hkpDisplayBindingDataPhysicsSystem(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpDisplayBindingDataRigidBody" => {
                        Ok(
                            Classes::hkpDisplayBindingDataRigidBody(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpEntity" => Ok(Classes::hkpEntity(next_boxed(&mut map)?)),
                    "hkpEntityExtendedListeners" => {
                        Ok(Classes::hkpEntityExtendedListeners(next_boxed(&mut map)?))
                    }
                    "hkpEntitySmallArraySerializeOverrideType" => {
                        Ok(
                            Classes::hkpEntitySmallArraySerializeOverrideType(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpEntitySpuCollisionCallback" => {
                        Ok(Classes::hkpEntitySpuCollisionCallback(next_boxed(&mut map)?))
                    }
                    "hkpExtendedMeshShape" => {
                        Ok(Classes::hkpExtendedMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpExtendedMeshShapeShapesSubpart" => {
                        Ok(
                            Classes::hkpExtendedMeshShapeShapesSubpart(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpExtendedMeshShapeSubpart" => {
                        Ok(Classes::hkpExtendedMeshShapeSubpart(next_boxed(&mut map)?))
                    }
                    "hkpExtendedMeshShapeTrianglesSubpart" => {
                        Ok(
                            Classes::hkpExtendedMeshShapeTrianglesSubpart(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpFastMeshShape" => {
                        Ok(Classes::hkpFastMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpFirstPersonGun" => {
                        Ok(Classes::hkpFirstPersonGun(next_boxed(&mut map)?))
                    }
                    "hkpFixedRigidMotion" => {
                        Ok(Classes::hkpFixedRigidMotion(next_boxed(&mut map)?))
                    }
                    "hkpGenericConstraintData" => {
                        Ok(Classes::hkpGenericConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpGenericConstraintDataScheme" => {
                        Ok(
                            Classes::hkpGenericConstraintDataScheme(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpGenericConstraintDataSchemeConstraintInfo" => {
                        Ok(
                            Classes::hkpGenericConstraintDataSchemeConstraintInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpGravityGun" => Ok(Classes::hkpGravityGun(next_boxed(&mut map)?)),
                    "hkpGroupCollisionFilter" => {
                        Ok(Classes::hkpGroupCollisionFilter(next_boxed(&mut map)?))
                    }
                    "hkpGroupFilter" => {
                        Ok(Classes::hkpGroupFilter(next_boxed(&mut map)?))
                    }
                    "hkpHeightFieldShape" => {
                        Ok(Classes::hkpHeightFieldShape(next_boxed(&mut map)?))
                    }
                    "hkpHingeConstraintData" => {
                        Ok(Classes::hkpHingeConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpHingeConstraintDataAtoms" => {
                        Ok(Classes::hkpHingeConstraintDataAtoms(next_boxed(&mut map)?))
                    }
                    "hkpHingeLimitsData" => {
                        Ok(Classes::hkpHingeLimitsData(next_boxed(&mut map)?))
                    }
                    "hkpHingeLimitsDataAtoms" => {
                        Ok(Classes::hkpHingeLimitsDataAtoms(next_boxed(&mut map)?))
                    }
                    "hkpIgnoreModifierConstraintAtom" => {
                        Ok(
                            Classes::hkpIgnoreModifierConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpKeyframedRigidMotion" => {
                        Ok(Classes::hkpKeyframedRigidMotion(next_boxed(&mut map)?))
                    }
                    "hkpLimitedForceConstraintMotor" => {
                        Ok(
                            Classes::hkpLimitedForceConstraintMotor(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpLimitedHingeConstraintData" => {
                        Ok(Classes::hkpLimitedHingeConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpLimitedHingeConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpLimitedHingeConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpLinConstraintAtom" => {
                        Ok(Classes::hkpLinConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpLinearParametricCurve" => {
                        Ok(Classes::hkpLinearParametricCurve(next_boxed(&mut map)?))
                    }
                    "hkpLinFrictionConstraintAtom" => {
                        Ok(Classes::hkpLinFrictionConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpLinkedCollidable" => {
                        Ok(Classes::hkpLinkedCollidable(next_boxed(&mut map)?))
                    }
                    "hkpLinLimitConstraintAtom" => {
                        Ok(Classes::hkpLinLimitConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpLinMotorConstraintAtom" => {
                        Ok(Classes::hkpLinMotorConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpLinSoftConstraintAtom" => {
                        Ok(Classes::hkpLinSoftConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpListShape" => Ok(Classes::hkpListShape(next_boxed(&mut map)?)),
                    "hkpListShapeChildInfo" => {
                        Ok(Classes::hkpListShapeChildInfo(next_boxed(&mut map)?))
                    }
                    "hkpMalleableConstraintData" => {
                        Ok(Classes::hkpMalleableConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpMassChangerModifierConstraintAtom" => {
                        Ok(
                            Classes::hkpMassChangerModifierConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpMassProperties" => {
                        Ok(Classes::hkpMassProperties(next_boxed(&mut map)?))
                    }
                    "hkpMaterial" => Ok(Classes::hkpMaterial(next_boxed(&mut map)?)),
                    "hkpMaxSizeMotion" => {
                        Ok(Classes::hkpMaxSizeMotion(next_boxed(&mut map)?))
                    }
                    "hkpMeshMaterial" => {
                        Ok(Classes::hkpMeshMaterial(next_boxed(&mut map)?))
                    }
                    "hkpMeshShape" => Ok(Classes::hkpMeshShape(next_boxed(&mut map)?)),
                    "hkpMeshShapeSubpart" => {
                        Ok(Classes::hkpMeshShapeSubpart(next_boxed(&mut map)?))
                    }
                    "hkpModifierConstraintAtom" => {
                        Ok(Classes::hkpModifierConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpMoppBvTreeShape" => {
                        Ok(Classes::hkpMoppBvTreeShape(next_boxed(&mut map)?))
                    }
                    "hkpMoppCode" => Ok(Classes::hkpMoppCode(next_boxed(&mut map)?)),
                    "hkpMoppCodeCodeInfo" => {
                        Ok(Classes::hkpMoppCodeCodeInfo(next_boxed(&mut map)?))
                    }
                    "hkpMoppCodeReindexedTerminal" => {
                        Ok(Classes::hkpMoppCodeReindexedTerminal(next_boxed(&mut map)?))
                    }
                    "hkpMotion" => Ok(Classes::hkpMotion(next_boxed(&mut map)?)),
                    "hkpMotorAction" => {
                        Ok(Classes::hkpMotorAction(next_boxed(&mut map)?))
                    }
                    "hkpMountedBallGun" => {
                        Ok(Classes::hkpMountedBallGun(next_boxed(&mut map)?))
                    }
                    "hkpMouseSpringAction" => {
                        Ok(Classes::hkpMouseSpringAction(next_boxed(&mut map)?))
                    }
                    "hkpMovingSurfaceModifierConstraintAtom" => {
                        Ok(
                            Classes::hkpMovingSurfaceModifierConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpMultiRayShape" => {
                        Ok(Classes::hkpMultiRayShape(next_boxed(&mut map)?))
                    }
                    "hkpMultiRayShapeRay" => {
                        Ok(Classes::hkpMultiRayShapeRay(next_boxed(&mut map)?))
                    }
                    "hkpMultiSphereShape" => {
                        Ok(Classes::hkpMultiSphereShape(next_boxed(&mut map)?))
                    }
                    "hkpMultithreadedVehicleManager" => {
                        Ok(
                            Classes::hkpMultithreadedVehicleManager(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpNamedMeshMaterial" => {
                        Ok(Classes::hkpNamedMeshMaterial(next_boxed(&mut map)?))
                    }
                    "hkpNullCollisionFilter" => {
                        Ok(Classes::hkpNullCollisionFilter(next_boxed(&mut map)?))
                    }
                    "hkPostFinishAttribute" => {
                        Ok(Classes::hkPostFinishAttribute(next_boxed(&mut map)?))
                    }
                    "hkpOverwritePivotConstraintAtom" => {
                        Ok(
                            Classes::hkpOverwritePivotConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpPairCollisionFilter" => {
                        Ok(Classes::hkpPairCollisionFilter(next_boxed(&mut map)?))
                    }
                    "hkpPairCollisionFilterMapPairFilterKeyOverrideType" => {
                        Ok(
                            Classes::hkpPairCollisionFilterMapPairFilterKeyOverrideType(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpParametricCurve" => {
                        Ok(Classes::hkpParametricCurve(next_boxed(&mut map)?))
                    }
                    "hkpPhantom" => Ok(Classes::hkpPhantom(next_boxed(&mut map)?)),
                    "hkpPhantomCallbackShape" => {
                        Ok(Classes::hkpPhantomCallbackShape(next_boxed(&mut map)?))
                    }
                    "hkpPhysicsData" => {
                        Ok(Classes::hkpPhysicsData(next_boxed(&mut map)?))
                    }
                    "hkpPhysicsSystem" => {
                        Ok(Classes::hkpPhysicsSystem(next_boxed(&mut map)?))
                    }
                    "hkpPhysicsSystemWithContacts" => {
                        Ok(Classes::hkpPhysicsSystemWithContacts(next_boxed(&mut map)?))
                    }
                    "hkpPlaneShape" => Ok(Classes::hkpPlaneShape(next_boxed(&mut map)?)),
                    "hkpPointToPathConstraintData" => {
                        Ok(Classes::hkpPointToPathConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpPointToPlaneConstraintData" => {
                        Ok(Classes::hkpPointToPlaneConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpPointToPlaneConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpPointToPlaneConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpPositionConstraintMotor" => {
                        Ok(Classes::hkpPositionConstraintMotor(next_boxed(&mut map)?))
                    }
                    "hkpPoweredChainData" => {
                        Ok(Classes::hkpPoweredChainData(next_boxed(&mut map)?))
                    }
                    "hkpPoweredChainDataConstraintInfo" => {
                        Ok(
                            Classes::hkpPoweredChainDataConstraintInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpPoweredChainMapper" => {
                        Ok(Classes::hkpPoweredChainMapper(next_boxed(&mut map)?))
                    }
                    "hkpPoweredChainMapperLinkInfo" => {
                        Ok(Classes::hkpPoweredChainMapperLinkInfo(next_boxed(&mut map)?))
                    }
                    "hkpPoweredChainMapperTarget" => {
                        Ok(Classes::hkpPoweredChainMapperTarget(next_boxed(&mut map)?))
                    }
                    "hkpPrismaticConstraintData" => {
                        Ok(Classes::hkpPrismaticConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpPrismaticConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpPrismaticConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpProjectileGun" => {
                        Ok(Classes::hkpProjectileGun(next_boxed(&mut map)?))
                    }
                    "hkpProperty" => Ok(Classes::hkpProperty(next_boxed(&mut map)?)),
                    "hkpPropertyValue" => {
                        Ok(Classes::hkpPropertyValue(next_boxed(&mut map)?))
                    }
                    "hkpPulleyConstraintAtom" => {
                        Ok(Classes::hkpPulleyConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpPulleyConstraintData" => {
                        Ok(Classes::hkpPulleyConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpPulleyConstraintDataAtoms" => {
                        Ok(Classes::hkpPulleyConstraintDataAtoms(next_boxed(&mut map)?))
                    }
                    "hkpRackAndPinionConstraintAtom" => {
                        Ok(
                            Classes::hkpRackAndPinionConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpRackAndPinionConstraintData" => {
                        Ok(
                            Classes::hkpRackAndPinionConstraintData(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpRackAndPinionConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpRackAndPinionConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpRagdollConstraintData" => {
                        Ok(Classes::hkpRagdollConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpRagdollConstraintDataAtoms" => {
                        Ok(Classes::hkpRagdollConstraintDataAtoms(next_boxed(&mut map)?))
                    }
                    "hkpRagdollLimitsData" => {
                        Ok(Classes::hkpRagdollLimitsData(next_boxed(&mut map)?))
                    }
                    "hkpRagdollLimitsDataAtoms" => {
                        Ok(Classes::hkpRagdollLimitsDataAtoms(next_boxed(&mut map)?))
                    }
                    "hkpRagdollMotorConstraintAtom" => {
                        Ok(Classes::hkpRagdollMotorConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpRayCollidableFilter" => {
                        Ok(Classes::hkpRayCollidableFilter(next_boxed(&mut map)?))
                    }
                    "hkpRayShapeCollectionFilter" => {
                        Ok(Classes::hkpRayShapeCollectionFilter(next_boxed(&mut map)?))
                    }
                    "hkpRejectChassisListener" => {
                        Ok(Classes::hkpRejectChassisListener(next_boxed(&mut map)?))
                    }
                    "hkpRemoveTerminalsMoppModifier" => {
                        Ok(
                            Classes::hkpRemoveTerminalsMoppModifier(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpReorientAction" => {
                        Ok(Classes::hkpReorientAction(next_boxed(&mut map)?))
                    }
                    "hkpRigidBody" => Ok(Classes::hkpRigidBody(next_boxed(&mut map)?)),
                    "hkpRotationalConstraintData" => {
                        Ok(Classes::hkpRotationalConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpRotationalConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpRotationalConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSampledHeightFieldShape" => {
                        Ok(Classes::hkpSampledHeightFieldShape(next_boxed(&mut map)?))
                    }
                    "hkpSerializedAgentNnEntry" => {
                        Ok(Classes::hkpSerializedAgentNnEntry(next_boxed(&mut map)?))
                    }
                    "hkpSerializedDisplayMarker" => {
                        Ok(Classes::hkpSerializedDisplayMarker(next_boxed(&mut map)?))
                    }
                    "hkpSerializedDisplayMarkerList" => {
                        Ok(
                            Classes::hkpSerializedDisplayMarkerList(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSerializedDisplayRbTransforms" => {
                        Ok(
                            Classes::hkpSerializedDisplayRbTransforms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSerializedDisplayRbTransformsDisplayTransformPair" => {
                        Ok(
                            Classes::hkpSerializedDisplayRbTransformsDisplayTransformPair(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSerializedSubTrack1nInfo" => {
                        Ok(Classes::hkpSerializedSubTrack1nInfo(next_boxed(&mut map)?))
                    }
                    "hkpSerializedTrack1nInfo" => {
                        Ok(Classes::hkpSerializedTrack1nInfo(next_boxed(&mut map)?))
                    }
                    "hkpSetLocalRotationsConstraintAtom" => {
                        Ok(
                            Classes::hkpSetLocalRotationsConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSetLocalTransformsConstraintAtom" => {
                        Ok(
                            Classes::hkpSetLocalTransformsConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSetLocalTranslationsConstraintAtom" => {
                        Ok(
                            Classes::hkpSetLocalTranslationsConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSetupStabilizationAtom" => {
                        Ok(Classes::hkpSetupStabilizationAtom(next_boxed(&mut map)?))
                    }
                    "hkpShape" => Ok(Classes::hkpShape(next_boxed(&mut map)?)),
                    "hkpShapeCollection" => {
                        Ok(Classes::hkpShapeCollection(next_boxed(&mut map)?))
                    }
                    "hkpShapeCollectionFilter" => {
                        Ok(Classes::hkpShapeCollectionFilter(next_boxed(&mut map)?))
                    }
                    "hkpShapeContainer" => {
                        Ok(Classes::hkpShapeContainer(next_boxed(&mut map)?))
                    }
                    "hkpShapeInfo" => Ok(Classes::hkpShapeInfo(next_boxed(&mut map)?)),
                    "hkpShapeModifier" => {
                        Ok(Classes::hkpShapeModifier(next_boxed(&mut map)?))
                    }
                    "hkpShapePhantom" => {
                        Ok(Classes::hkpShapePhantom(next_boxed(&mut map)?))
                    }
                    "hkpSimpleContactConstraintAtom" => {
                        Ok(
                            Classes::hkpSimpleContactConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSimpleContactConstraintDataInfo" => {
                        Ok(
                            Classes::hkpSimpleContactConstraintDataInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSimpleMeshShape" => {
                        Ok(Classes::hkpSimpleMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpSimpleMeshShapeTriangle" => {
                        Ok(Classes::hkpSimpleMeshShapeTriangle(next_boxed(&mut map)?))
                    }
                    "hkpSimpleShapePhantom" => {
                        Ok(Classes::hkpSimpleShapePhantom(next_boxed(&mut map)?))
                    }
                    "hkpSimpleShapePhantomCollisionDetail" => {
                        Ok(
                            Classes::hkpSimpleShapePhantomCollisionDetail(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSimulation" => Ok(Classes::hkpSimulation(next_boxed(&mut map)?)),
                    "hkpSingleShapeContainer" => {
                        Ok(Classes::hkpSingleShapeContainer(next_boxed(&mut map)?))
                    }
                    "hkpSoftContactModifierConstraintAtom" => {
                        Ok(
                            Classes::hkpSoftContactModifierConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpSphereMotion" => {
                        Ok(Classes::hkpSphereMotion(next_boxed(&mut map)?))
                    }
                    "hkpSphereRepShape" => {
                        Ok(Classes::hkpSphereRepShape(next_boxed(&mut map)?))
                    }
                    "hkpSphereShape" => {
                        Ok(Classes::hkpSphereShape(next_boxed(&mut map)?))
                    }
                    "hkpSpringAction" => {
                        Ok(Classes::hkpSpringAction(next_boxed(&mut map)?))
                    }
                    "hkpSpringDamperConstraintMotor" => {
                        Ok(
                            Classes::hkpSpringDamperConstraintMotor(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStiffSpringChainData" => {
                        Ok(Classes::hkpStiffSpringChainData(next_boxed(&mut map)?))
                    }
                    "hkpStiffSpringChainDataConstraintInfo" => {
                        Ok(
                            Classes::hkpStiffSpringChainDataConstraintInfo(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStiffSpringConstraintAtom" => {
                        Ok(Classes::hkpStiffSpringConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpStiffSpringConstraintData" => {
                        Ok(Classes::hkpStiffSpringConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpStiffSpringConstraintDataAtoms" => {
                        Ok(
                            Classes::hkpStiffSpringConstraintDataAtoms(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStorageExtendedMeshShape" => {
                        Ok(Classes::hkpStorageExtendedMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpStorageExtendedMeshShapeMaterial" => {
                        Ok(
                            Classes::hkpStorageExtendedMeshShapeMaterial(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStorageExtendedMeshShapeMeshSubpartStorage" => {
                        Ok(
                            Classes::hkpStorageExtendedMeshShapeMeshSubpartStorage(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStorageExtendedMeshShapeShapeSubpartStorage" => {
                        Ok(
                            Classes::hkpStorageExtendedMeshShapeShapeSubpartStorage(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStorageMeshShape" => {
                        Ok(Classes::hkpStorageMeshShape(next_boxed(&mut map)?))
                    }
                    "hkpStorageMeshShapeSubpartStorage" => {
                        Ok(
                            Classes::hkpStorageMeshShapeSubpartStorage(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpStorageSampledHeightFieldShape" => {
                        Ok(
                            Classes::hkpStorageSampledHeightFieldShape(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpThinBoxMotion" => {
                        Ok(Classes::hkpThinBoxMotion(next_boxed(&mut map)?))
                    }
                    "hkpTransformShape" => {
                        Ok(Classes::hkpTransformShape(next_boxed(&mut map)?))
                    }
                    "hkpTriangleShape" => {
                        Ok(Classes::hkpTriangleShape(next_boxed(&mut map)?))
                    }
                    "hkpTriggerVolume" => {
                        Ok(Classes::hkpTriggerVolume(next_boxed(&mut map)?))
                    }
                    "hkpTriggerVolumeEventInfo" => {
                        Ok(Classes::hkpTriggerVolumeEventInfo(next_boxed(&mut map)?))
                    }
                    "hkpTriSampledHeightFieldBvTreeShape" => {
                        Ok(
                            Classes::hkpTriSampledHeightFieldBvTreeShape(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpTriSampledHeightFieldCollection" => {
                        Ok(
                            Classes::hkpTriSampledHeightFieldCollection(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpTwistLimitConstraintAtom" => {
                        Ok(Classes::hkpTwistLimitConstraintAtom(next_boxed(&mut map)?))
                    }
                    "hkpTypedBroadPhaseHandle" => {
                        Ok(Classes::hkpTypedBroadPhaseHandle(next_boxed(&mut map)?))
                    }
                    "hkpTyremarkPoint" => {
                        Ok(Classes::hkpTyremarkPoint(next_boxed(&mut map)?))
                    }
                    "hkpTyremarksInfo" => {
                        Ok(Classes::hkpTyremarksInfo(next_boxed(&mut map)?))
                    }
                    "hkpTyremarksWheel" => {
                        Ok(Classes::hkpTyremarksWheel(next_boxed(&mut map)?))
                    }
                    "hkpUnaryAction" => {
                        Ok(Classes::hkpUnaryAction(next_boxed(&mut map)?))
                    }
                    "hkpVehicleAerodynamics" => {
                        Ok(Classes::hkpVehicleAerodynamics(next_boxed(&mut map)?))
                    }
                    "hkpVehicleBrake" => {
                        Ok(Classes::hkpVehicleBrake(next_boxed(&mut map)?))
                    }
                    "hkpVehicleCastBatchingManager" => {
                        Ok(Classes::hkpVehicleCastBatchingManager(next_boxed(&mut map)?))
                    }
                    "hkpVehicleData" => {
                        Ok(Classes::hkpVehicleData(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDataWheelComponentParams" => {
                        Ok(
                            Classes::hkpVehicleDataWheelComponentParams(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleDefaultAerodynamics" => {
                        Ok(Classes::hkpVehicleDefaultAerodynamics(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDefaultAnalogDriverInput" => {
                        Ok(
                            Classes::hkpVehicleDefaultAnalogDriverInput(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleDefaultBrake" => {
                        Ok(Classes::hkpVehicleDefaultBrake(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDefaultBrakeWheelBrakingProperties" => {
                        Ok(
                            Classes::hkpVehicleDefaultBrakeWheelBrakingProperties(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleDefaultEngine" => {
                        Ok(Classes::hkpVehicleDefaultEngine(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDefaultSteering" => {
                        Ok(Classes::hkpVehicleDefaultSteering(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDefaultSuspension" => {
                        Ok(Classes::hkpVehicleDefaultSuspension(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters" => {
                        Ok(
                            Classes::hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleDefaultTransmission" => {
                        Ok(Classes::hkpVehicleDefaultTransmission(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDefaultVelocityDamper" => {
                        Ok(
                            Classes::hkpVehicleDefaultVelocityDamper(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleDriverInput" => {
                        Ok(Classes::hkpVehicleDriverInput(next_boxed(&mut map)?))
                    }
                    "hkpVehicleDriverInputAnalogStatus" => {
                        Ok(
                            Classes::hkpVehicleDriverInputAnalogStatus(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleDriverInputStatus" => {
                        Ok(Classes::hkpVehicleDriverInputStatus(next_boxed(&mut map)?))
                    }
                    "hkpVehicleEngine" => {
                        Ok(Classes::hkpVehicleEngine(next_boxed(&mut map)?))
                    }
                    "hkpVehicleFrictionDescription" => {
                        Ok(Classes::hkpVehicleFrictionDescription(next_boxed(&mut map)?))
                    }
                    "hkpVehicleFrictionDescriptionAxisDescription" => {
                        Ok(
                            Classes::hkpVehicleFrictionDescriptionAxisDescription(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleFrictionStatus" => {
                        Ok(Classes::hkpVehicleFrictionStatus(next_boxed(&mut map)?))
                    }
                    "hkpVehicleFrictionStatusAxisStatus" => {
                        Ok(
                            Classes::hkpVehicleFrictionStatusAxisStatus(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleInstance" => {
                        Ok(Classes::hkpVehicleInstance(next_boxed(&mut map)?))
                    }
                    "hkpVehicleInstanceWheelInfo" => {
                        Ok(Classes::hkpVehicleInstanceWheelInfo(next_boxed(&mut map)?))
                    }
                    "hkpVehicleLinearCastBatchingManager" => {
                        Ok(
                            Classes::hkpVehicleLinearCastBatchingManager(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleLinearCastWheelCollide" => {
                        Ok(
                            Classes::hkpVehicleLinearCastWheelCollide(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleLinearCastWheelCollideWheelState" => {
                        Ok(
                            Classes::hkpVehicleLinearCastWheelCollideWheelState(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleManager" => {
                        Ok(Classes::hkpVehicleManager(next_boxed(&mut map)?))
                    }
                    "hkpVehicleRayCastBatchingManager" => {
                        Ok(
                            Classes::hkpVehicleRayCastBatchingManager(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleRayCastWheelCollide" => {
                        Ok(Classes::hkpVehicleRayCastWheelCollide(next_boxed(&mut map)?))
                    }
                    "hkpVehicleSteering" => {
                        Ok(Classes::hkpVehicleSteering(next_boxed(&mut map)?))
                    }
                    "hkpVehicleSuspension" => {
                        Ok(Classes::hkpVehicleSuspension(next_boxed(&mut map)?))
                    }
                    "hkpVehicleSuspensionSuspensionWheelParameters" => {
                        Ok(
                            Classes::hkpVehicleSuspensionSuspensionWheelParameters(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpVehicleTransmission" => {
                        Ok(Classes::hkpVehicleTransmission(next_boxed(&mut map)?))
                    }
                    "hkpVehicleVelocityDamper" => {
                        Ok(Classes::hkpVehicleVelocityDamper(next_boxed(&mut map)?))
                    }
                    "hkpVehicleWheelCollide" => {
                        Ok(Classes::hkpVehicleWheelCollide(next_boxed(&mut map)?))
                    }
                    "hkpVelocityConstraintMotor" => {
                        Ok(Classes::hkpVelocityConstraintMotor(next_boxed(&mut map)?))
                    }
                    "hkpViscousSurfaceModifierConstraintAtom" => {
                        Ok(
                            Classes::hkpViscousSurfaceModifierConstraintAtom(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkpWeldingUtility" => {
                        Ok(Classes::hkpWeldingUtility(next_boxed(&mut map)?))
                    }
                    "hkpWheelConstraintData" => {
                        Ok(Classes::hkpWheelConstraintData(next_boxed(&mut map)?))
                    }
                    "hkpWheelConstraintDataAtoms" => {
                        Ok(Classes::hkpWheelConstraintDataAtoms(next_boxed(&mut map)?))
                    }
                    "hkpWorld" => Ok(Classes::hkpWorld(next_boxed(&mut map)?)),
                    "hkpWorldCinfo" => Ok(Classes::hkpWorldCinfo(next_boxed(&mut map)?)),
                    "hkpWorldObject" => {
                        Ok(Classes::hkpWorldObject(next_boxed(&mut map)?))
                    }
                    "hkQTransform" => Ok(Classes::hkQTransform(next_boxed(&mut map)?)),
                    "hkRangeInt32Attribute" => {
                        Ok(Classes::hkRangeInt32Attribute(next_boxed(&mut map)?))
                    }
                    "hkRangeRealAttribute" => {
                        Ok(Classes::hkRangeRealAttribute(next_boxed(&mut map)?))
                    }
                    "hkReferencedObject" => {
                        Ok(Classes::hkReferencedObject(next_boxed(&mut map)?))
                    }
                    "hkReflectedFileAttribute" => {
                        Ok(Classes::hkReflectedFileAttribute(next_boxed(&mut map)?))
                    }
                    "hkResourceBase" => {
                        Ok(Classes::hkResourceBase(next_boxed(&mut map)?))
                    }
                    "hkResourceContainer" => {
                        Ok(Classes::hkResourceContainer(next_boxed(&mut map)?))
                    }
                    "hkResourceHandle" => {
                        Ok(Classes::hkResourceHandle(next_boxed(&mut map)?))
                    }
                    "hkRootLevelContainer" => {
                        Ok(Classes::hkRootLevelContainer(next_boxed(&mut map)?))
                    }
                    "hkRootLevelContainerNamedVariant" => {
                        Ok(
                            Classes::hkRootLevelContainerNamedVariant(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkSemanticsAttribute" => {
                        Ok(Classes::hkSemanticsAttribute(next_boxed(&mut map)?))
                    }
                    "hkSimpleLocalFrame" => {
                        Ok(Classes::hkSimpleLocalFrame(next_boxed(&mut map)?))
                    }
                    "hkSphere" => Ok(Classes::hkSphere(next_boxed(&mut map)?)),
                    "hkSweptTransform" => {
                        Ok(Classes::hkSweptTransform(next_boxed(&mut map)?))
                    }
                    "hkTraceStreamTitle" => {
                        Ok(Classes::hkTraceStreamTitle(next_boxed(&mut map)?))
                    }
                    "hkTrackerSerializableScanSnapshot" => {
                        Ok(
                            Classes::hkTrackerSerializableScanSnapshot(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkTrackerSerializableScanSnapshotAllocation" => {
                        Ok(
                            Classes::hkTrackerSerializableScanSnapshotAllocation(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkTrackerSerializableScanSnapshotBlock" => {
                        Ok(
                            Classes::hkTrackerSerializableScanSnapshotBlock(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkUiAttribute" => Ok(Classes::hkUiAttribute(next_boxed(&mut map)?)),
                    "hkVertexFormat" => {
                        Ok(Classes::hkVertexFormat(next_boxed(&mut map)?))
                    }
                    "hkVertexFormatElement" => {
                        Ok(Classes::hkVertexFormatElement(next_boxed(&mut map)?))
                    }
                    "hkWorldMemoryAvailableWatchDog" => {
                        Ok(
                            Classes::hkWorldMemoryAvailableWatchDog(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkxAnimatedFloat" => {
                        Ok(Classes::hkxAnimatedFloat(next_boxed(&mut map)?))
                    }
                    "hkxAnimatedMatrix" => {
                        Ok(Classes::hkxAnimatedMatrix(next_boxed(&mut map)?))
                    }
                    "hkxAnimatedQuaternion" => {
                        Ok(Classes::hkxAnimatedQuaternion(next_boxed(&mut map)?))
                    }
                    "hkxAnimatedVector" => {
                        Ok(Classes::hkxAnimatedVector(next_boxed(&mut map)?))
                    }
                    "hkxAttribute" => Ok(Classes::hkxAttribute(next_boxed(&mut map)?)),
                    "hkxAttributeGroup" => {
                        Ok(Classes::hkxAttributeGroup(next_boxed(&mut map)?))
                    }
                    "hkxAttributeHolder" => {
                        Ok(Classes::hkxAttributeHolder(next_boxed(&mut map)?))
                    }
                    "hkxCamera" => Ok(Classes::hkxCamera(next_boxed(&mut map)?)),
                    "hkxEdgeSelectionChannel" => {
                        Ok(Classes::hkxEdgeSelectionChannel(next_boxed(&mut map)?))
                    }
                    "hkxEnum" => Ok(Classes::hkxEnum(next_boxed(&mut map)?)),
                    "hkxEnumItem" => Ok(Classes::hkxEnumItem(next_boxed(&mut map)?)),
                    "hkxEnvironment" => {
                        Ok(Classes::hkxEnvironment(next_boxed(&mut map)?))
                    }
                    "hkxEnvironmentVariable" => {
                        Ok(Classes::hkxEnvironmentVariable(next_boxed(&mut map)?))
                    }
                    "hkxIndexBuffer" => {
                        Ok(Classes::hkxIndexBuffer(next_boxed(&mut map)?))
                    }
                    "hkxLight" => Ok(Classes::hkxLight(next_boxed(&mut map)?)),
                    "hkxMaterial" => Ok(Classes::hkxMaterial(next_boxed(&mut map)?)),
                    "hkxMaterialEffect" => {
                        Ok(Classes::hkxMaterialEffect(next_boxed(&mut map)?))
                    }
                    "hkxMaterialProperty" => {
                        Ok(Classes::hkxMaterialProperty(next_boxed(&mut map)?))
                    }
                    "hkxMaterialShader" => {
                        Ok(Classes::hkxMaterialShader(next_boxed(&mut map)?))
                    }
                    "hkxMaterialShaderSet" => {
                        Ok(Classes::hkxMaterialShaderSet(next_boxed(&mut map)?))
                    }
                    "hkxMaterialTextureStage" => {
                        Ok(Classes::hkxMaterialTextureStage(next_boxed(&mut map)?))
                    }
                    "hkxMesh" => Ok(Classes::hkxMesh(next_boxed(&mut map)?)),
                    "hkxMeshSection" => {
                        Ok(Classes::hkxMeshSection(next_boxed(&mut map)?))
                    }
                    "hkxMeshUserChannelInfo" => {
                        Ok(Classes::hkxMeshUserChannelInfo(next_boxed(&mut map)?))
                    }
                    "hkxNode" => Ok(Classes::hkxNode(next_boxed(&mut map)?)),
                    "hkxNodeAnnotationData" => {
                        Ok(Classes::hkxNodeAnnotationData(next_boxed(&mut map)?))
                    }
                    "hkxNodeSelectionSet" => {
                        Ok(Classes::hkxNodeSelectionSet(next_boxed(&mut map)?))
                    }
                    "hkxScene" => Ok(Classes::hkxScene(next_boxed(&mut map)?)),
                    "hkxSkinBinding" => {
                        Ok(Classes::hkxSkinBinding(next_boxed(&mut map)?))
                    }
                    "hkxSparselyAnimatedBool" => {
                        Ok(Classes::hkxSparselyAnimatedBool(next_boxed(&mut map)?))
                    }
                    "hkxSparselyAnimatedEnum" => {
                        Ok(Classes::hkxSparselyAnimatedEnum(next_boxed(&mut map)?))
                    }
                    "hkxSparselyAnimatedInt" => {
                        Ok(Classes::hkxSparselyAnimatedInt(next_boxed(&mut map)?))
                    }
                    "hkxSparselyAnimatedString" => {
                        Ok(Classes::hkxSparselyAnimatedString(next_boxed(&mut map)?))
                    }
                    "hkxTextureFile" => {
                        Ok(Classes::hkxTextureFile(next_boxed(&mut map)?))
                    }
                    "hkxTextureInplace" => {
                        Ok(Classes::hkxTextureInplace(next_boxed(&mut map)?))
                    }
                    "hkxTriangleSelectionChannel" => {
                        Ok(Classes::hkxTriangleSelectionChannel(next_boxed(&mut map)?))
                    }
                    "hkxVertexBuffer" => {
                        Ok(Classes::hkxVertexBuffer(next_boxed(&mut map)?))
                    }
                    "hkxVertexBufferVertexData" => {
                        Ok(Classes::hkxVertexBufferVertexData(next_boxed(&mut map)?))
                    }
                    "hkxVertexDescription" => {
                        Ok(Classes::hkxVertexDescription(next_boxed(&mut map)?))
                    }
                    "hkxVertexDescriptionElementDecl" => {
                        Ok(
                            Classes::hkxVertexDescriptionElementDecl(
                                next_boxed(&mut map)?,
                            ),
                        )
                    }
                    "hkxVertexFloatDataChannel" => {
                        Ok(Classes::hkxVertexFloatDataChannel(next_boxed(&mut map)?))
                    }
                    "hkxVertexIntDataChannel" => {
                        Ok(Classes::hkxVertexIntDataChannel(next_boxed(&mut map)?))
                    }
                    "hkxVertexSelectionChannel" => {
                        Ok(Classes::hkxVertexSelectionChannel(next_boxed(&mut map)?))
                    }
                    "hkxVertexVectorDataChannel" => {
                        Ok(Classes::hkxVertexVectorDataChannel(next_boxed(&mut map)?))
                    }
                    _ => {
                        Err(
                            _serde::de::Error::unknown_field(
                                class_name,
                                &[
                                    "BGSGamebryoSequenceGenerator",
                                    "BSBoneSwitchGenerator",
                                    "BSBoneSwitchGeneratorBoneData",
                                    "BSComputeAddBoneAnimModifier",
                                    "BSCyclicBlendTransitionGenerator",
                                    "BSDecomposeVectorModifier",
                                    "BSDirectAtModifier",
                                    "BSDistTriggerModifier",
                                    "BSEventEveryNEventsModifier",
                                    "BSEventOnDeactivateModifier",
                                    "BSEventOnFalseToTrueModifier",
                                    "BSGetTimeStepModifier",
                                    "BSInterpValueModifier",
                                    "BSIsActiveModifier",
                                    "BSIStateManagerModifier",
                                    "BSIStateManagerModifierBSiStateData",
                                    "BSIStateManagerModifierBSIStateManagerStateListener",
                                    "BSiStateTaggingGenerator",
                                    "BSLimbIKModifier",
                                    "BSLookAtModifier",
                                    "BSLookAtModifierBoneData",
                                    "BSModifyOnceModifier",
                                    "BSOffsetAnimationGenerator",
                                    "BSPassByTargetTriggerModifier",
                                    "BSRagdollContactListenerModifier",
                                    "BSSpeedSamplerModifier",
                                    "BSSynchronizedClipGenerator",
                                    "BSTimerModifier",
                                    "BSTweenerModifier",
                                    "hkAabb",
                                    "hkAabbHalf",
                                    "hkAabbUint32",
                                    "hkaAnimatedReferenceFrame",
                                    "hkaAnimation",
                                    "hkaAnimationBinding",
                                    "hkaAnimationContainer",
                                    "hkaAnimationPreviewColorContainer",
                                    "hkaAnnotationTrack",
                                    "hkaAnnotationTrackAnnotation",
                                    "hkaBone",
                                    "hkaBoneAttachment",
                                    "hkaDefaultAnimatedReferenceFrame",
                                    "hkaDeltaCompressedAnimation",
                                    "hkaDeltaCompressedAnimationQuantizationFormat",
                                    "hkaFootstepAnalysisInfo",
                                    "hkaFootstepAnalysisInfoContainer",
                                    "hkaInterleavedUncompressedAnimation",
                                    "hkaKeyFrameHierarchyUtility",
                                    "hkaKeyFrameHierarchyUtilityControlData",
                                    "hkAlignSceneToNodeOptions",
                                    "hkaMeshBinding",
                                    "hkaMeshBindingMapping",
                                    "hkaQuantizedAnimation",
                                    "hkaQuantizedAnimationTrackCompressionParams",
                                    "hkaRagdollInstance",
                                    "hkArrayTypeAttribute",
                                    "hkaSkeleton",
                                    "hkaSkeletonLocalFrameOnBone",
                                    "hkaSkeletonMapper",
                                    "hkaSkeletonMapperData",
                                    "hkaSkeletonMapperDataChainMapping",
                                    "hkaSkeletonMapperDataSimpleMapping",
                                    "hkaSplineCompressedAnimation",
                                    "hkaSplineCompressedAnimationAnimationCompressionParams",
                                    "hkaSplineCompressedAnimationTrackCompressionParams",
                                    "hkaWaveletCompressedAnimation",
                                    "hkaWaveletCompressedAnimationCompressionParams",
                                    "hkaWaveletCompressedAnimationQuantizationFormat",
                                    "hkBaseObject",
                                    "hkbAttachmentModifier",
                                    "hkbAttachmentSetup",
                                    "hkbAttributeModifier",
                                    "hkbAttributeModifierAssignment",
                                    "hkbAuxiliaryNodeInfo",
                                    "hkbBehaviorEventsInfo",
                                    "hkbBehaviorGraph",
                                    "hkbBehaviorGraphData",
                                    "hkbBehaviorGraphInternalState",
                                    "hkbBehaviorGraphInternalStateInfo",
                                    "hkbBehaviorGraphStringData",
                                    "hkbBehaviorInfo",
                                    "hkbBehaviorInfoIdToNamePair",
                                    "hkbBehaviorReferenceGenerator",
                                    "hkbBindable",
                                    "hkbBlendCurveUtils",
                                    "hkbBlenderGenerator",
                                    "hkbBlenderGeneratorChild",
                                    "hkbBlenderGeneratorChildInternalState",
                                    "hkbBlenderGeneratorInternalState",
                                    "hkbBlendingTransitionEffect",
                                    "hkbBlendingTransitionEffectInternalState",
                                    "hkbBoneIndexArray",
                                    "hkbBoneWeightArray",
                                    "hkbBoolVariableSequencedData",
                                    "hkbBoolVariableSequencedDataSample",
                                    "hkbCameraShakeEventPayload",
                                    "hkbCharacter",
                                    "hkbCharacterAddedInfo",
                                    "hkbCharacterControlCommand",
                                    "hkbCharacterControllerControlData",
                                    "hkbCharacterControllerModifier",
                                    "hkbCharacterControllerModifierInternalState",
                                    "hkbCharacterData",
                                    "hkbCharacterDataCharacterControllerInfo",
                                    "hkbCharacterInfo",
                                    "hkbCharacterSetup",
                                    "hkbCharacterSkinInfo",
                                    "hkbCharacterSteppedInfo",
                                    "hkbCharacterStringData",
                                    "hkbClientCharacterState",
                                    "hkbClipGenerator",
                                    "hkbClipGeneratorEcho",
                                    "hkbClipGeneratorInternalState",
                                    "hkbClipTrigger",
                                    "hkbClipTriggerArray",
                                    "hkbCombineTransformsModifier",
                                    "hkbCombineTransformsModifierInternalState",
                                    "hkbCompiledExpressionSet",
                                    "hkbCompiledExpressionSetToken",
                                    "hkbComputeDirectionModifier",
                                    "hkbComputeDirectionModifierInternalState",
                                    "hkbComputeRotationFromAxisAngleModifier",
                                    "hkbComputeRotationFromAxisAngleModifierInternalState",
                                    "hkbComputeRotationToTargetModifier",
                                    "hkbComputeRotationToTargetModifierInternalState",
                                    "hkbCondition",
                                    "hkbContext",
                                    "hkbDampingModifier",
                                    "hkbDampingModifierInternalState",
                                    "hkbDefaultMessageLog",
                                    "hkbDelayedModifier",
                                    "hkbDelayedModifierInternalState",
                                    "hkbDetectCloseToGroundModifier",
                                    "hkbDetectCloseToGroundModifierInternalState",
                                    "hkbEvaluateExpressionModifier",
                                    "hkbEvaluateExpressionModifierInternalExpressionData",
                                    "hkbEvaluateExpressionModifierInternalState",
                                    "hkbEvaluateHandleModifier",
                                    "hkbEvent",
                                    "hkbEventBase",
                                    "hkbEventDrivenModifier",
                                    "hkbEventDrivenModifierInternalState",
                                    "hkbEventInfo",
                                    "hkbEventPayload",
                                    "hkbEventPayloadList",
                                    "hkbEventProperty",
                                    "hkbEventRaisedInfo",
                                    "hkbEventRangeData",
                                    "hkbEventRangeDataArray",
                                    "hkbEventSequencedData",
                                    "hkbEventSequencedDataSequencedEvent",
                                    "hkbEventsFromRangeModifier",
                                    "hkbEventsFromRangeModifierInternalState",
                                    "hkbExpressionCondition",
                                    "hkbExpressionData",
                                    "hkbExpressionDataArray",
                                    "hkbExtractRagdollPoseModifier",
                                    "hkbFootIkControlData",
                                    "hkbFootIkControlsModifier",
                                    "hkbFootIkControlsModifierLeg",
                                    "hkbFootIkDriverInfo",
                                    "hkbFootIkDriverInfoLeg",
                                    "hkbFootIkGains",
                                    "hkbFootIkModifier",
                                    "hkbFootIkModifierInternalLegData",
                                    "hkbFootIkModifierLeg",
                                    "hkbGenerator",
                                    "hkbGeneratorOutputListener",
                                    "hkbGeneratorSyncInfo",
                                    "hkbGeneratorSyncInfoSyncPoint",
                                    "hkbGeneratorTransitionEffect",
                                    "hkbGeneratorTransitionEffectInternalState",
                                    "hkbGetHandleOnBoneModifier",
                                    "hkbGetUpModifier",
                                    "hkbGetUpModifierInternalState",
                                    "hkbGetWorldFromModelModifier",
                                    "hkbGetWorldFromModelModifierInternalState",
                                    "hkbHandIkControlData",
                                    "hkbHandIkControlsModifier",
                                    "hkbHandIkControlsModifierHand",
                                    "hkbHandIkDriverInfo",
                                    "hkbHandIkDriverInfoHand",
                                    "hkbHandIkModifier",
                                    "hkbHandIkModifierHand",
                                    "hkbHandle",
                                    "hkbIntEventPayload",
                                    "hkbIntVariableSequencedData",
                                    "hkbIntVariableSequencedDataSample",
                                    "hkBitField",
                                    "hkbKeyframeBonesModifier",
                                    "hkbKeyframeBonesModifierKeyframeInfo",
                                    "hkbLinkedSymbolInfo",
                                    "hkbLookAtModifier",
                                    "hkbLookAtModifierInternalState",
                                    "hkbManualSelectorGenerator",
                                    "hkbManualSelectorGeneratorInternalState",
                                    "hkbMessageLog",
                                    "hkbMirroredSkeletonInfo",
                                    "hkbMirrorModifier",
                                    "hkbModifier",
                                    "hkbModifierGenerator",
                                    "hkbModifierList",
                                    "hkbModifierWrapper",
                                    "hkbMoveCharacterModifier",
                                    "hkbMoveCharacterModifierInternalState",
                                    "hkbNamedEventPayload",
                                    "hkbNamedIntEventPayload",
                                    "hkbNamedRealEventPayload",
                                    "hkbNamedStringEventPayload",
                                    "hkbNode",
                                    "hkbNodeInternalStateInfo",
                                    "hkbParticleSystemEventPayload",
                                    "hkbPoseMatchingGenerator",
                                    "hkbPoseMatchingGeneratorInternalState",
                                    "hkbPoweredRagdollControlData",
                                    "hkbPoweredRagdollControlsModifier",
                                    "hkbProjectData",
                                    "hkbProjectStringData",
                                    "hkbProxyModifier",
                                    "hkbProxyModifierProxyInfo",
                                    "hkbRaiseEventCommand",
                                    "hkbRealEventPayload",
                                    "hkbRealVariableSequencedData",
                                    "hkbRealVariableSequencedDataSample",
                                    "hkbReferencePoseGenerator",
                                    "hkbRegisteredGenerator",
                                    "hkbRigidBodyRagdollControlData",
                                    "hkbRigidBodyRagdollControlsModifier",
                                    "hkbRoleAttribute",
                                    "hkbRotateCharacterModifier",
                                    "hkbRotateCharacterModifierInternalState",
                                    "hkbSenseHandleModifier",
                                    "hkbSenseHandleModifierRange",
                                    "hkbSequence",
                                    "hkbSequencedData",
                                    "hkbSequenceInternalState",
                                    "hkbSequenceStringData",
                                    "hkbSetBehaviorCommand",
                                    "hkbSetLocalTimeOfClipGeneratorCommand",
                                    "hkbSetNodePropertyCommand",
                                    "hkbSetWordVariableCommand",
                                    "hkbSetWorldFromModelModifier",
                                    "hkbSimulationControlCommand",
                                    "hkbSimulationStateInfo",
                                    "hkbStateChooser",
                                    "hkbStateListener",
                                    "hkbStateMachine",
                                    "hkbStateMachineActiveTransitionInfo",
                                    "hkbStateMachineDelayedTransitionInfo",
                                    "hkbStateMachineEventPropertyArray",
                                    "hkbStateMachineInternalState",
                                    "hkbStateMachineNestedStateMachineData",
                                    "hkbStateMachineProspectiveTransitionInfo",
                                    "hkbStateMachineStateInfo",
                                    "hkbStateMachineTimeInterval",
                                    "hkbStateMachineTransitionInfo",
                                    "hkbStateMachineTransitionInfoArray",
                                    "hkbStateMachineTransitionInfoReference",
                                    "hkbStringCondition",
                                    "hkbStringEventPayload",
                                    "hkbTestStateChooser",
                                    "hkbTimerModifier",
                                    "hkbTimerModifierInternalState",
                                    "hkbTransformVectorModifier",
                                    "hkbTransformVectorModifierInternalState",
                                    "hkbTransitionEffect",
                                    "hkbTwistModifier",
                                    "hkbVariableBindingSet",
                                    "hkbVariableBindingSetBinding",
                                    "hkbVariableInfo",
                                    "hkbVariableValue",
                                    "hkbVariableValueSet",
                                    "hkbWorldEnums",
                                    "hkbWorldFromModelModeData",
                                    "hkClass",
                                    "hkClassEnum",
                                    "hkClassEnumItem",
                                    "hkClassMember",
                                    "hkColor",
                                    "hkContactPoint",
                                    "hkContactPointMaterial",
                                    "hkCustomAttributes",
                                    "hkCustomAttributesAttribute",
                                    "hkDataObjectTypeAttribute",
                                    "hkDescriptionAttribute",
                                    "hkDocumentationAttribute",
                                    "hkGeometry",
                                    "hkGeometryTriangle",
                                    "hkGizmoAttribute",
                                    "hkHalf8",
                                    "hkIndexedTransformSet",
                                    "hkLinkAttribute",
                                    "hkLocalFrame",
                                    "hkLocalFrameGroup",
                                    "hkMemoryMeshBody",
                                    "hkMemoryMeshMaterial",
                                    "hkMemoryMeshShape",
                                    "hkMemoryMeshTexture",
                                    "hkMemoryMeshVertexBuffer",
                                    "hkMemoryResourceContainer",
                                    "hkMemoryResourceHandle",
                                    "hkMemoryResourceHandleExternalLink",
                                    "hkMemoryTrackerAttribute",
                                    "hkMeshBody",
                                    "hkMeshBoneIndexMapping",
                                    "hkMeshMaterial",
                                    "hkMeshSection",
                                    "hkMeshSectionCinfo",
                                    "hkMeshShape",
                                    "hkMeshTexture",
                                    "hkMeshVertexBuffer",
                                    "hkModelerNodeTypeAttribute",
                                    "hkMonitorStreamColorTable",
                                    "hkMonitorStreamColorTableColorPair",
                                    "hkMonitorStreamFrameInfo",
                                    "hkMonitorStreamStringMap",
                                    "hkMonitorStreamStringMapStringMap",
                                    "hkMoppBvTreeShapeBase",
                                    "hkMotionState",
                                    "hkMultipleVertexBuffer",
                                    "hkMultipleVertexBufferElementInfo",
                                    "hkMultipleVertexBufferLockedElement",
                                    "hkMultipleVertexBufferVertexBufferInfo",
                                    "hkMultiThreadCheck",
                                    "hkp2dAngConstraintAtom",
                                    "hkpAabbPhantom",
                                    "hkPackedVector3",
                                    "hkPackfileHeader",
                                    "hkPackfileSectionHeader",
                                    "hkpAction",
                                    "hkpAgent1nSector",
                                    "hkpAngConstraintAtom",
                                    "hkpAngFrictionConstraintAtom",
                                    "hkpAngLimitConstraintAtom",
                                    "hkpAngMotorConstraintAtom",
                                    "hkpAngularDashpotAction",
                                    "hkpArrayAction",
                                    "hkpBallAndSocketConstraintData",
                                    "hkpBallAndSocketConstraintDataAtoms",
                                    "hkpBallGun",
                                    "hkpBallSocketChainData",
                                    "hkpBallSocketChainDataConstraintInfo",
                                    "hkpBallSocketConstraintAtom",
                                    "hkpBinaryAction",
                                    "hkpBoxMotion",
                                    "hkpBoxShape",
                                    "hkpBreakableBody",
                                    "hkpBreakableConstraintData",
                                    "hkpBridgeAtoms",
                                    "hkpBridgeConstraintAtom",
                                    "hkpBroadPhaseHandle",
                                    "hkpBvShape",
                                    "hkpBvTreeShape",
                                    "hkpCachingShapePhantom",
                                    "hkpCallbackConstraintMotor",
                                    "hkpCapsuleShape",
                                    "hkpCdBody",
                                    "hkpCenterOfMassChangerModifierConstraintAtom",
                                    "hkpCharacterControllerCinfo",
                                    "hkpCharacterMotion",
                                    "hkpCharacterProxyCinfo",
                                    "hkpCharacterRigidBodyCinfo",
                                    "hkpCogWheelConstraintAtom",
                                    "hkpCogWheelConstraintData",
                                    "hkpCogWheelConstraintDataAtoms",
                                    "hkpCollidable",
                                    "hkpCollidableBoundingVolumeData",
                                    "hkpCollidableCollidableFilter",
                                    "hkpCollisionFilter",
                                    "hkpCollisionFilterList",
                                    "hkpCompressedMeshShape",
                                    "hkpCompressedMeshShapeBigTriangle",
                                    "hkpCompressedMeshShapeChunk",
                                    "hkpCompressedMeshShapeConvexPiece",
                                    "hkpCompressedSampledHeightFieldShape",
                                    "hkpConeLimitConstraintAtom",
                                    "hkpConstrainedSystemFilter",
                                    "hkpConstraintAtom",
                                    "hkpConstraintChainData",
                                    "hkpConstraintChainInstance",
                                    "hkpConstraintChainInstanceAction",
                                    "hkpConstraintCollisionFilter",
                                    "hkpConstraintData",
                                    "hkpConstraintInstance",
                                    "hkpConstraintInstanceSmallArraySerializeOverrideType",
                                    "hkpConstraintMotor",
                                    "hkpConvexListFilter",
                                    "hkpConvexListShape",
                                    "hkpConvexPieceMeshShape",
                                    "hkpConvexPieceStreamData",
                                    "hkpConvexShape",
                                    "hkpConvexTransformShape",
                                    "hkpConvexTransformShapeBase",
                                    "hkpConvexTranslateShape",
                                    "hkpConvexVerticesConnectivity",
                                    "hkpConvexVerticesShape",
                                    "hkpConvexVerticesShapeFourVectors",
                                    "hkpCylinderShape",
                                    "hkpDashpotAction",
                                    "hkpDefaultConvexListFilter",
                                    "hkpDefaultWorldMemoryWatchDog",
                                    "hkpDisableEntityCollisionFilter",
                                    "hkpDisplayBindingData",
                                    "hkpDisplayBindingDataPhysicsSystem",
                                    "hkpDisplayBindingDataRigidBody",
                                    "hkpEntity",
                                    "hkpEntityExtendedListeners",
                                    "hkpEntitySmallArraySerializeOverrideType",
                                    "hkpEntitySpuCollisionCallback",
                                    "hkpExtendedMeshShape",
                                    "hkpExtendedMeshShapeShapesSubpart",
                                    "hkpExtendedMeshShapeSubpart",
                                    "hkpExtendedMeshShapeTrianglesSubpart",
                                    "hkpFastMeshShape",
                                    "hkpFirstPersonGun",
                                    "hkpFixedRigidMotion",
                                    "hkpGenericConstraintData",
                                    "hkpGenericConstraintDataScheme",
                                    "hkpGenericConstraintDataSchemeConstraintInfo",
                                    "hkpGravityGun",
                                    "hkpGroupCollisionFilter",
                                    "hkpGroupFilter",
                                    "hkpHeightFieldShape",
                                    "hkpHingeConstraintData",
                                    "hkpHingeConstraintDataAtoms",
                                    "hkpHingeLimitsData",
                                    "hkpHingeLimitsDataAtoms",
                                    "hkpIgnoreModifierConstraintAtom",
                                    "hkpKeyframedRigidMotion",
                                    "hkpLimitedForceConstraintMotor",
                                    "hkpLimitedHingeConstraintData",
                                    "hkpLimitedHingeConstraintDataAtoms",
                                    "hkpLinConstraintAtom",
                                    "hkpLinearParametricCurve",
                                    "hkpLinFrictionConstraintAtom",
                                    "hkpLinkedCollidable",
                                    "hkpLinLimitConstraintAtom",
                                    "hkpLinMotorConstraintAtom",
                                    "hkpLinSoftConstraintAtom",
                                    "hkpListShape",
                                    "hkpListShapeChildInfo",
                                    "hkpMalleableConstraintData",
                                    "hkpMassChangerModifierConstraintAtom",
                                    "hkpMassProperties",
                                    "hkpMaterial",
                                    "hkpMaxSizeMotion",
                                    "hkpMeshMaterial",
                                    "hkpMeshShape",
                                    "hkpMeshShapeSubpart",
                                    "hkpModifierConstraintAtom",
                                    "hkpMoppBvTreeShape",
                                    "hkpMoppCode",
                                    "hkpMoppCodeCodeInfo",
                                    "hkpMoppCodeReindexedTerminal",
                                    "hkpMotion",
                                    "hkpMotorAction",
                                    "hkpMountedBallGun",
                                    "hkpMouseSpringAction",
                                    "hkpMovingSurfaceModifierConstraintAtom",
                                    "hkpMultiRayShape",
                                    "hkpMultiRayShapeRay",
                                    "hkpMultiSphereShape",
                                    "hkpMultithreadedVehicleManager",
                                    "hkpNamedMeshMaterial",
                                    "hkpNullCollisionFilter",
                                    "hkPostFinishAttribute",
                                    "hkpOverwritePivotConstraintAtom",
                                    "hkpPairCollisionFilter",
                                    "hkpPairCollisionFilterMapPairFilterKeyOverrideType",
                                    "hkpParametricCurve",
                                    "hkpPhantom",
                                    "hkpPhantomCallbackShape",
                                    "hkpPhysicsData",
                                    "hkpPhysicsSystem",
                                    "hkpPhysicsSystemWithContacts",
                                    "hkpPlaneShape",
                                    "hkpPointToPathConstraintData",
                                    "hkpPointToPlaneConstraintData",
                                    "hkpPointToPlaneConstraintDataAtoms",
                                    "hkpPositionConstraintMotor",
                                    "hkpPoweredChainData",
                                    "hkpPoweredChainDataConstraintInfo",
                                    "hkpPoweredChainMapper",
                                    "hkpPoweredChainMapperLinkInfo",
                                    "hkpPoweredChainMapperTarget",
                                    "hkpPrismaticConstraintData",
                                    "hkpPrismaticConstraintDataAtoms",
                                    "hkpProjectileGun",
                                    "hkpProperty",
                                    "hkpPropertyValue",
                                    "hkpPulleyConstraintAtom",
                                    "hkpPulleyConstraintData",
                                    "hkpPulleyConstraintDataAtoms",
                                    "hkpRackAndPinionConstraintAtom",
                                    "hkpRackAndPinionConstraintData",
                                    "hkpRackAndPinionConstraintDataAtoms",
                                    "hkpRagdollConstraintData",
                                    "hkpRagdollConstraintDataAtoms",
                                    "hkpRagdollLimitsData",
                                    "hkpRagdollLimitsDataAtoms",
                                    "hkpRagdollMotorConstraintAtom",
                                    "hkpRayCollidableFilter",
                                    "hkpRayShapeCollectionFilter",
                                    "hkpRejectChassisListener",
                                    "hkpRemoveTerminalsMoppModifier",
                                    "hkpReorientAction",
                                    "hkpRigidBody",
                                    "hkpRotationalConstraintData",
                                    "hkpRotationalConstraintDataAtoms",
                                    "hkpSampledHeightFieldShape",
                                    "hkpSerializedAgentNnEntry",
                                    "hkpSerializedDisplayMarker",
                                    "hkpSerializedDisplayMarkerList",
                                    "hkpSerializedDisplayRbTransforms",
                                    "hkpSerializedDisplayRbTransformsDisplayTransformPair",
                                    "hkpSerializedSubTrack1nInfo",
                                    "hkpSerializedTrack1nInfo",
                                    "hkpSetLocalRotationsConstraintAtom",
                                    "hkpSetLocalTransformsConstraintAtom",
                                    "hkpSetLocalTranslationsConstraintAtom",
                                    "hkpSetupStabilizationAtom",
                                    "hkpShape",
                                    "hkpShapeCollection",
                                    "hkpShapeCollectionFilter",
                                    "hkpShapeContainer",
                                    "hkpShapeInfo",
                                    "hkpShapeModifier",
                                    "hkpShapePhantom",
                                    "hkpSimpleContactConstraintAtom",
                                    "hkpSimpleContactConstraintDataInfo",
                                    "hkpSimpleMeshShape",
                                    "hkpSimpleMeshShapeTriangle",
                                    "hkpSimpleShapePhantom",
                                    "hkpSimpleShapePhantomCollisionDetail",
                                    "hkpSimulation",
                                    "hkpSingleShapeContainer",
                                    "hkpSoftContactModifierConstraintAtom",
                                    "hkpSphereMotion",
                                    "hkpSphereRepShape",
                                    "hkpSphereShape",
                                    "hkpSpringAction",
                                    "hkpSpringDamperConstraintMotor",
                                    "hkpStiffSpringChainData",
                                    "hkpStiffSpringChainDataConstraintInfo",
                                    "hkpStiffSpringConstraintAtom",
                                    "hkpStiffSpringConstraintData",
                                    "hkpStiffSpringConstraintDataAtoms",
                                    "hkpStorageExtendedMeshShape",
                                    "hkpStorageExtendedMeshShapeMaterial",
                                    "hkpStorageExtendedMeshShapeMeshSubpartStorage",
                                    "hkpStorageExtendedMeshShapeShapeSubpartStorage",
                                    "hkpStorageMeshShape",
                                    "hkpStorageMeshShapeSubpartStorage",
                                    "hkpStorageSampledHeightFieldShape",
                                    "hkpThinBoxMotion",
                                    "hkpTransformShape",
                                    "hkpTriangleShape",
                                    "hkpTriggerVolume",
                                    "hkpTriggerVolumeEventInfo",
                                    "hkpTriSampledHeightFieldBvTreeShape",
                                    "hkpTriSampledHeightFieldCollection",
                                    "hkpTwistLimitConstraintAtom",
                                    "hkpTypedBroadPhaseHandle",
                                    "hkpTyremarkPoint",
                                    "hkpTyremarksInfo",
                                    "hkpTyremarksWheel",
                                    "hkpUnaryAction",
                                    "hkpVehicleAerodynamics",
                                    "hkpVehicleBrake",
                                    "hkpVehicleCastBatchingManager",
                                    "hkpVehicleData",
                                    "hkpVehicleDataWheelComponentParams",
                                    "hkpVehicleDefaultAerodynamics",
                                    "hkpVehicleDefaultAnalogDriverInput",
                                    "hkpVehicleDefaultBrake",
                                    "hkpVehicleDefaultBrakeWheelBrakingProperties",
                                    "hkpVehicleDefaultEngine",
                                    "hkpVehicleDefaultSteering",
                                    "hkpVehicleDefaultSuspension",
                                    "hkpVehicleDefaultSuspensionWheelSpringSuspensionParameters",
                                    "hkpVehicleDefaultTransmission",
                                    "hkpVehicleDefaultVelocityDamper",
                                    "hkpVehicleDriverInput",
                                    "hkpVehicleDriverInputAnalogStatus",
                                    "hkpVehicleDriverInputStatus",
                                    "hkpVehicleEngine",
                                    "hkpVehicleFrictionDescription",
                                    "hkpVehicleFrictionDescriptionAxisDescription",
                                    "hkpVehicleFrictionStatus",
                                    "hkpVehicleFrictionStatusAxisStatus",
                                    "hkpVehicleInstance",
                                    "hkpVehicleInstanceWheelInfo",
                                    "hkpVehicleLinearCastBatchingManager",
                                    "hkpVehicleLinearCastWheelCollide",
                                    "hkpVehicleLinearCastWheelCollideWheelState",
                                    "hkpVehicleManager",
                                    "hkpVehicleRayCastBatchingManager",
                                    "hkpVehicleRayCastWheelCollide",
                                    "hkpVehicleSteering",
                                    "hkpVehicleSuspension",
                                    "hkpVehicleSuspensionSuspensionWheelParameters",
                                    "hkpVehicleTransmission",
                                    "hkpVehicleVelocityDamper",
                                    "hkpVehicleWheelCollide",
                                    "hkpVelocityConstraintMotor",
                                    "hkpViscousSurfaceModifierConstraintAtom",
                                    "hkpWeldingUtility",
                                    "hkpWheelConstraintData",
                                    "hkpWheelConstraintDataAtoms",
                                    "hkpWorld",
                                    "hkpWorldCinfo",
                                    "hkpWorldObject",
                                    "hkQTransform",
                                    "hkRangeInt32Attribute",
                                    "hkRangeRealAttribute",
                                    "hkReferencedObject",
                                    "hkReflectedFileAttribute",
                                    "hkResourceBase",
                                    "hkResourceContainer",
                                    "hkResourceHandle",
                                    "hkRootLevelContainer",
                                    "hkRootLevelContainerNamedVariant",
                                    "hkSemanticsAttribute",
                                    "hkSimpleLocalFrame",
                                    "hkSphere",
                                    "hkSweptTransform",
                                    "hkTraceStreamTitle",
                                    "hkTrackerSerializableScanSnapshot",
                                    "hkTrackerSerializableScanSnapshotAllocation",
                                    "hkTrackerSerializableScanSnapshotBlock",
                                    "hkUiAttribute",
                                    "hkVertexFormat",
                                    "hkVertexFormatElement",
                                    "hkWorldMemoryAvailableWatchDog",
                                    "hkxAnimatedFloat",
                                    "hkxAnimatedMatrix",
                                    "hkxAnimatedQuaternion",
                                    "hkxAnimatedVector",
                                    "hkxAttribute",
                                    "hkxAttributeGroup",
                                    "hkxAttributeHolder",
                                    "hkxCamera",
                                    "hkxEdgeSelectionChannel",
                                    "hkxEnum",
                                    "hkxEnumItem",
                                    "hkxEnvironment",
                                    "hkxEnvironmentVariable",
                                    "hkxIndexBuffer",
                                    "hkxLight",
                                    "hkxMaterial",
                                    "hkxMaterialEffect",
                                    "hkxMaterialProperty",
                                    "hkxMaterialShader",
                                    "hkxMaterialShaderSet",
                                    "hkxMaterialTextureStage",
                                    "hkxMesh",
                                    "hkxMeshSection",
                                    "hkxMeshUserChannelInfo",
                                    "hkxNode",
                                    "hkxNodeAnnotationData",
                                    "hkxNodeSelectionSet",
                                    "hkxScene",
                                    "hkxSkinBinding",
                                    "hkxSparselyAnimatedBool",
                                    "hkxSparselyAnimatedEnum",
                                    "hkxSparselyAnimatedInt",
                                    "hkxSparselyAnimatedString",
                                    "hkxTextureFile",
                                    "hkxTextureInplace",
                                    "hkxTriangleSelectionChannel",
                                    "hkxVertexBuffer",
                                    "hkxVertexBufferVertexData",
                                    "hkxVertexDescription",
                                    "hkxVertexDescriptionElementDecl",
                                    "hkxVertexFloatDataChannel",
                                    "hkxVertexIntDataChannel",
                                    "hkxVertexSelectionChannel",
                                    "hkxVertexVectorDataChannel",
                                ],
                            ),
                        )
                    }
                }
            }
        }
        deserializer
            .deserialize_class_index(ClassesVisitor {
                marker: core::marker::PhantomData,
            })
    }
}
