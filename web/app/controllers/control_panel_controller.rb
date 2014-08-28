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
    rpc = Rails.application.get_rpc
    @news = rpc.call.ch_newsfeed_api.read_index(session[:member_clan]) || []
    @news.sort_by! { |item| item[:created] }
  end

  def newsfeed_add_form

  end

  def acl
    @clan_members = []
    session[:clan_info]['members'].each do |_, member|
      @clan_members.append [member['account_name'], member['account_id']]
    end
  end
end
