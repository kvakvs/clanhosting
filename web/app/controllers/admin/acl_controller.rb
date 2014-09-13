class Admin::AclController < ApplicationController
  include ApplicationHelper
  before_action :pre_check_site_exists!

  def index
    return unless require_clan_admin

    ## member[0] is user name, member[1] is user id
    @clan_members = ClanModel::get_members_helper session[:clan_info]
    @clan_acl = {}
    @clan_members.each do |member|
      @clan_acl[member[1]] = AclModel::read_for_user(session[:user_clan], member[1]) || []
    end
  end
end
