class ControlPanelController < ApplicationController
  before_action :pre_check_site_exists!
  before_action :pre_check_acl!

  def pre_check_site_exists!
    unless session[:clan_site_exists]
      redirect_to root_path, :alert => t('app.site_is_not_created_notice')
    end
  end

  def pre_check_acl!
    # TODO: Replace with proper ACL check
    unless session[:is_clan_leader]
      redirect_to root_path, :alert => t('acl.no_rights_to_see_this')
    end
  end

  def index

  end

  def newsfeed
    clan = session[:user_clan]
    rpc = Rails.application.get_rpc
    @news = rpc.call.ch_newsfeed_api.read_index(clan) || []
    @news.map! { |item_id| rpc.call.ch_newsfeed_api.read_one(clan, item_id) }
  end

  def newsfeed_add_form
  end

  def newsfeed_add
    return redirect_to manage_newsfeed_add_form_path,
                       :alert => 'nil in title' if params[:title].nil?
    return redirect_to manage_newsfeed_add_form_path,
                       :alert => 'nil in short' if params[:short].nil?
    return redirect_to manage_newsfeed_add_form_path,
                       :alert => 'nil in long' if params[:long].nil?
    fields = {:title => params[:title],
              :short => params[:short],
              :long  => params[:long] }
    rpc = Rails.application.get_rpc
    rpc.call.ch_newsfeed_api.add_post(session[:user_clan], fields)
    redirect_to manage_newsfeed_path
  end

  def acl
    @clan_members = []
    session[:clan_info]['members'].each do |_, member|
      @clan_members.append [member['account_name'], member['account_id']]
    end
  end
end
