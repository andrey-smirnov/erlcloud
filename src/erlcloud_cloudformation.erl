-module (erlcloud_cloudformation).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "2010-05-15").

-type access_key_id() :: string().
-type secret_access_key() :: string().

-type params() :: proplists:proplist().
-type cloudformation_list() :: proplists:proplist().

%% Library initialization
-export([
    configure/2,
    new/2
]).


%% Cloud Formation API Functions
-export ([list_stacks_all/2,
          list_stacks/2,
          list_stack_resources_all/2,
          list_stack_resources/2,
          describe_stack_resources/2,
          describe_stack_resource/2,
          describe_stacks_all/2,
          describe_stacks/2,
          get_stack_policy/2,
          get_template/2,
          get_template_summary/2,
          describe_account_limits/2,
          describe_account_limits_all/1,
          describe_stack_events_all/2,
          describe_stack_events/2]).


%%==============================================================================
%% Library initialization
%%==============================================================================

-spec configure(access_key_id(), secret_access_key()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.


-spec new(access_key_id(), secret_access_key()) -> #aws_config{}.
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.

%%==============================================================================
%% Cloud Formation API Functions
%%==============================================================================

-spec list_stacks_all(params(), aws_config()) -> {ok, cloudformation_list()}.
list_stacks_all(Params, Config = #aws_config{}) ->
    list_all(fun list_stacks/2, Params, Config, []).

-spec list_stacks(params(), aws_config()) -> {ok, cloudformation_list()}.
list_stacks(Params, Config = #aws_config{}) ->

    ExtraParams = lists:map(fun(T) ->
            case T of
                {stack_status_filter, N, Filter} ->
                    {io_lib:format("StackStatusFilter.member.~p", [N]), Filter};

                _ -> T
            end
        end, Params),

    {ok, XmlNode} = cloudformation_request(Config, "ListStacks", ExtraParams),

    NextToken = erlcloud_xml:get_text("/ListStacksResponse/ListStacksResult/NextToken", XmlNode, undefined),

    [{next_token, NextToken}, extract_stack_summaries(xmerl_xpath:string("/ListStacksResponse/ListStacksResult", XmlNode))].

-spec list_stack_resources_all(string(), aws_config()) -> {ok, cloudformation_list()}.
list_stack_resources_all(Params, Config = #aws_config{}) ->
    list_all(fun list_stack_resources/2, Params, Config, []).

-spec list_stack_resources(params(), aws_config()) -> {ok, cloudformation_list()}.
list_stack_resources(Params, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "ListStackResources", Params),

    NextToken = erlcloud_xml:get_text("/ListStackResourcesResponse/ListStackResourcesResult/NextToken", XmlNode, undefined),

    [{next_token, NextToken}, extract_list_stack_resources(xmerl_xpath:string("/ListStackResourcesResponse/ListStackResourcesResult", XmlNode))].

-spec describe_stack_resources(string(), aws_config()) -> {ok, cloudformation_list()}.
describe_stack_resources(Params, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "DescribeStackResources", Params),
    extract_stack_resources_members(xmerl_xpath:string("/DescribeStackResourcesResponse/DescribeStackResourcesResult", XmlNode)).

-spec describe_stack_resource(params(), aws_config) -> {ok, cloudformation_list()}.
describe_stack_resource(Param, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "DescribeStackResource", Param),
    extract_stack_details(xmerl_xpath:string("/DescribeStackResourceResponse/DescribeStackResourceResult", XmlNode)).


-spec describe_stacks_all(params(), aws_config()) -> {ok, cloudformation_list()}.
describe_stacks_all(Params, Config = #aws_config{}) ->
    list_all(fun describe_stacks/2, Params, Config, []).

-spec describe_stacks(params(), aws_config()) -> {ok, cloudformation_list()}.
describe_stacks(Params, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "DescribeStacks", Params),

    NextToken = erlcloud_xml:get_text("/DescribeStacksResponse/DescribeStacksResult/NextToken", XmlNode, undefined),

    [{next_token, NextToken}, extract_described_stacks_result(xmerl_xpath:string("/DescribeStacksResponse/DescribeStacksResult", XmlNode))].

-spec get_stack_policy(params(), aws_config()) -> {ok, cloudformation_list()}.
get_stack_policy(Params, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "GetStackPolicy", Params),

    extract_stack_policy_body(xmerl_xpath:string("/GetStackPolicyResponse", XmlNode)).


-spec describe_stack_events_all(params(), aws_config()) -> {ok, cloudformation_list()}.
describe_stack_events_all(Params, Config = #aws_config{}) ->
    list_all(fun describe_stack_events/2, Params, Config, []).

-spec describe_stack_events(params(), aws_config()) -> {ok, cloudformation_list()}.
describe_stack_events(Params, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "DescribeStackEvents", Params),

    NextToken = erlcloud_xml:get_text("/DescribeStackEventsResponse/DescribeStackEventsResult/NextToken", XmlNode, undefined),

    [{next_token, NextToken}, extract_described_stack_events_result(xmerl_xpath:string("/DescribeStackEventsResponse/DescribeStackEventsResult", XmlNode))].

-spec get_template(string(), aws_config()) -> {ok, cloudformation_list()}.
get_template(StackName, Config = #aws_config{}) ->
    {ok, XmlNode} = cloudformation_request(Config, "GetTemplate", [{"StackName", StackName}]),
    extract_template_response(XmlNode).

-spec get_template_summary(params(), aws_config()) -> {ok, cloudformation_list()}.
get_template_summary(Params, Config = #aws_config{}) ->

    {ok, XmlNodes} = cloudformation_request(Config, "GetTemplateSummary", Params),
    extract_template_summary_response(XmlNodes).

-spec describe_account_limits_all(aws_config()) -> {ok, cloudformation_list()}.
describe_account_limits_all(Config = #aws_config{}) ->
    list_all(fun describe_account_limits/2, [], Config, []).

-spec describe_account_limits(aws_config(), string()) -> {ok, cloudformation_list()}.
describe_account_limits(Params, Config = #aws_config{}) ->
    {ok, XmlNodes} = cloudformation_request(Config, "DescribeAccountLimits", Params),
    NextToken = erlcloud_xml:get_text("/DescribeAccountLimitsResponse/DescribeAccountLimitsResult/NextToken", XmlNodes, undefined),
    [{next_token, NextToken},  extract_accout_limits_response(xmerl_xpath:string("/DescribeAccountLimitsResponse", XmlNodes))].

%%==============================================================================
%% Internal functions
%%==============================================================================

cloudformation_request(Config = #aws_config{}, Action, ExtraParams) ->

    QParams = [
        {"Action", Action},
        {"Version", ?API_VERSION}
        | ExtraParams],

    erlcloud_aws:aws_request_xml4(post, Config#aws_config.cloudformation_host, "/", QParams, "cloudformation", Config).

list_all(Fun, Params, Config, Acc) ->
    List = Fun(Params, Config),
    case proplists:get_value(next_token, List, not_here) of
        undefined ->
            lists:foldl(fun erlang:'++'/2, [], [List | Acc]);
        NextToken ->
            list_all(Fun, [{"NextToken", NextToken} | Params], Config, [List | Acc])
    end.

extract_stack_summaries(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([{summaries, "StackSummaries", {optional_map, fun extract_stacks/1}}], T) end, XmlNodes).

extract_stacks(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {optional_map, fun extract_stack/1}}
        ], XmlNode).

extract_stack(XmlNode) ->
    erlcloud_xml:decode([
        {stack_id, "StackId", optional_text},
        {stack_status, "StackStatus", optional_text},
        {stack_name, "StackName", optional_text},
        {creation_time, "CreationTime", optional_text},
        {template_description, "TemplateDescription", optional_text},
        {resource_types, "ResourceTypes", list}], XmlNode).

extract_list_stack_resources(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
        {summaries, "StackResourceSummaries", {optional_map, fun extract_list_resources/1}}
        ], T) end, XmlNodes).

extract_list_resources(XmlNode) ->
    erlcloud_xml:decode([
        {member, "member", {optional_map, fun extract_list_resource/1}}
        ], XmlNode).

extract_list_resource(XmlNode) ->
    erlcloud_xml:decode([
        {resource_status, "ResourceStatus", optional_text},
        {logical_resource_id, "LogicalResourceId", optional_text},
        {last_updated_timestamp, "LastUpdatedTimestamp", optional_text},
        {physical_resource_id, "PhysicalResourceId", optional_text},
        {resource_type, "ResourceType", optional_text}
        ], XmlNode).

extract_stack_resources_members(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([{resources, "StackResources", {optional_map, fun extract_member_resources/1}}], T) end, XmlNodes).

extract_member_resources(XmlNode) ->
    erlcloud_xml:decode([{member, "member", {optional_map, fun extract_resource/1}}], XmlNode).

extract_resource(XmlNode) ->
    erlcloud_xml:decode([
        {stack_id, "StackId", optional_text},
        {stack_name, "StackName", optional_text},
        {logical_resource_id, "LogicalResourceId", optional_text},
        {physical_resource_id, "PhysicalResourceId", optional_text},
        {resource_type, "ResourceType", optional_text},
        {timestamp, "Timestamp", optional_text},
        {resource_status, "ResourceStatus", optional_text}
        ], XmlNode).

extract_stack_details(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([{resources, "StackResourceDetail", {optional_map, fun extract_resource/1}}], T) end, XmlNodes).

extract_described_stacks_result(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([{stacks, "Stacks", {optional_map, fun extract_described_stacks/1}}], T) end, XmlNodes).

extract_described_stacks(XmlNode) ->
    erlcloud_xml:decode([{member, "member", {optional_map, fun extract_described_stack/1}}], XmlNode).

extract_described_stack(XmlNode) ->
    erlcloud_xml:decode([
            {stack_name, "StackName", optional_text},
            {stack_id, "StackId", optional_text},
            {creation_time, "CreationTime", optional_text},
            {stack_status, "StackStatus", optional_text},
            {disable_rollback, "DisableRollback", optional_text},
            {outputs, "Outputs", {optional_map, fun extract_resource_outputs/1}}
        ], XmlNode).

extract_resource_outputs(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {optional_map, fun extract_resource_output/1}}
        ], XmlNode).

extract_resource_output(XmlNode) ->
    erlcloud_xml:decode([
            {output_key, "OutputKey", optional_text},
            {output_value, "OutputValue", optional_text}
        ], XmlNode).

extract_stack_policy_body(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
                {stack_policy_body, "GetStackPolicyResult", {optional_map, fun extract_policy_body/1}},
                {response_meta, "ResponseMetadata", {optional_map, fun extract_template_meta_body/1}}
            ], T) end, XmlNodes).

extract_policy_body(XmlNodes) ->
    erlcloud_xml:decode([
            {stack_policy_body, "StackPolicyBody", optional_text}
        ], XmlNodes).

extract_described_stack_events_result(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
                {stack_events, "StackEvents", {optional_map, fun extract_stack_event_member/1}}
            ], T) end, XmlNodes).

extract_stack_event_member(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {optional_map, fun extract_stack_event/1}}
    ], XmlNode).

extract_stack_event(XmlNode) ->
    erlcloud_xml:decode([
            {event_id, "EventId", optional_text},
            {stack_id, "StackId", optional_text},
            {stack_name, "StackName", optional_text},
            {logical_resource_id, "LogicalResourceId", optional_text},
            {physical_resource_id, "PhysicalResourceId", optional_text},
            {resource_type, "ResourceType", optional_text},
            {timestamp, "TimeStamp", optional_text},
            {resource_status, "ResourceStatus", optional_text},
            {resource_properties, "ResourceProperties", optional_text}
    ], XmlNode).

extract_template_response(XmlNodes) ->
    erlcloud_xml:decode([
                {template_result, "GetTemplateResult", {optional_map, fun extract_template_result/1}},
                {response_meta, "ResponseMetadata", {optional_map, fun extract_template_meta_body/1}}
            ], XmlNodes).

extract_template_meta_body(XmlNode) ->
    erlcloud_xml:decode([{request_id, "RequestId", optional_text}], XmlNode).

extract_template_result(XmlNode) ->
    erlcloud_xml:decode([{template_body, "TemplateBody", optional_text}], XmlNode).

extract_template_summary_response(XmlNodes) ->
    erlcloud_xml:decode([
                {template_summary_result, "GetTemplateSummaryResult", {optional_map, fun extract_template_summary_result/1}},
                {response_meta, "ResponseMetadata", {optional_map, fun extract_template_meta_body/1}}
        ], XmlNodes).

extract_template_summary_result(XmlNode) ->
    erlcloud_xml:decode([
                {description, "Description", optional_text},
                {parameters, "Parameters", {optional_map, fun extract_template_parameters/1}},
                {metadata, "Metadata", optional_text},
                {version, "Version", optional_text}
        ], XmlNode).

extract_template_parameters(XmlNode) ->
    erlcloud_xml:decode([
                {member, "member", {optional_map, fun extract_template_parameter/1}}
        ], XmlNode).

extract_template_parameter(XmlNode) ->
    erlcloud_xml:decode([
                {no_echo, "NoEcho", optional_text},
                {parameter_key, "ParameterKey", optional_text},
                {description, "Description", optional_text},
                {parameter_type, "ParameterType", optional_text}
        ], XmlNode).

extract_accout_limits_response(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
            {describe_account_limits_result, "DescribeAccountLimitsResult", {optional_map, fun extract_account_limits_result/1}}
        ], T) end, XmlNodes).

extract_account_limits_result(XmlNode) ->
    erlcloud_xml:decode([
            {account_limits, "AccountLimits", {optional_map, fun extract_account_limits/1}},
            {response_meta, "ResponseMetadata", {optional_map, fun extract_template_meta_body/1}}
        ], XmlNode).

extract_account_limits(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {optional_map, fun extract_account_limit/1}}
        ], XmlNode).

extract_account_limit(XmlNode) ->
    erlcloud_xml:decode([
            {name, "Name", optional_text},
            {value, "Value", optional_text}
        ], XmlNode).




