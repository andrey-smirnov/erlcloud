-module (erlcloud_cloudformation).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "2010-05-15").

-type params() :: proplists:proplist().
-type cloudformation_list() :: proplists:proplist().

%% Cloud Formation API Functions
-export ([list_stacks_all/2,
          list_stacks/2,
          list_stack_resources_all/2,
          list_stack_resources/2,
          describe_stack_resources/2,
          describe_stack_resource/2,
          describe_stacks_all/2,
          describe_stacks/3,
          get_stack_policy/2,
          get_template/2,
          get_template_summary/2,
          describe_account_limits/2,
          describe_account_limits_all/1,
          describe_stack_events_all/2,
          describe_stack_events/3]).

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
    % cloudformation_request(Config, "ListStackResources", [{"StackName", StackName}]).
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

    ExtraParams = lists:map(fun(T) ->
        case T of
            {stack_name, StackName} -> {"StackName", StackName}
        end
    end, Params),

    cloudformation_request(Config, "DescribeStacks", ExtraParams).

-spec describe_stacks(params(), string(), aws_config()) -> {ok, cloudformation_list()}.
describe_stacks(Params, NextNode, Config = #aws_config{}) ->

    ExtraParams = lists:map(fun(T) ->
        case T of
            {stack_name, StackName} -> {"StackName", StackName}
        end
    end, Params),

    cloudformation_request(Config, "DescribeStacks", [{"NextNode", NextNode}, ExtraParams]).

-spec get_stack_policy(string(), aws_config()) -> {ok, cloudformation_list()}.
get_stack_policy(StackName, Config = #aws_config{}) ->
    cloudformation_request(Config, "GetStackPolicy", [{"StackName", StackName}]).

-spec describe_stack_events_all(params(), aws_config()) -> {ok, cloudformation_list()}.
describe_stack_events_all(Params, Config = #aws_config{}) ->

    ExtraParams = lists:map(fun(T) ->
        case T of
            {stack_name, StackName} -> {"StackName", StackName}
        end
    end, Params),

    cloudformation_request(Config, "DescribeStackEvents", ExtraParams).

-spec describe_stack_events(params(), string(), aws_config()) -> {ok, cloudformation_list()}.
describe_stack_events(Params, NextToken, Config = #aws_config{}) ->

    ExtraParams = lists:map(fun(T) ->
        case T of
            {stack_name, StackName} -> {"StackName", StackName}
        end
    end, Params),

    cloudformation_request(Config, "DescribeStackEvents", [{"NextToken", NextToken} | ExtraParams]).

-spec get_template(string(), aws_config()) -> {ok, cloudformation_list()}.
get_template(StackName, Config = #aws_config{}) ->

    cloudformation_request(Config, "GetTemplate", [{"StackName", StackName}]).

-spec get_template_summary(params(), aws_config()) -> {ok, cloudformation_list()}.
get_template_summary(Params, Config = #aws_config{}) ->

    ExtraParams = lists:map(fun(T) ->
        case T of
            {template_url, URL} -> {"TemplateURL", URL};
            {template_body, Body} -> {"TemplateBody", Body};
            {stack_name, StackName} -> {"StackName", StackName}
        end
    end, Params),

    cloudformation_request(Config, "GetTemplateSummary", ExtraParams).

-spec describe_account_limits_all(aws_config()) -> {ok, cloudformation_list()}.
describe_account_limits_all(Config = #aws_config{}) ->

    cloudformation_request(Config, "DescribeAccountLimits", []).

-spec describe_account_limits(aws_config(), string()) -> {ok, cloudformation_list()}.
describe_account_limits(Config = #aws_config{}, NextToken) ->
    cloudformation_request(Config, "DescribeAccountLimits", [{"NextToken", NextToken}]).

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



